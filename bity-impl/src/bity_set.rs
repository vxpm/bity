use super::error;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{spanned::Spanned, Data, DeriveInput, LitInt};

pub fn generate(bit_size: LitInt, input: DeriveInput) -> Result<TokenStream, TokenStream> {
    let Data::Enum(enum_data) = input.data else {
        unreachable!()
    };

    let inner_ty = format_ident!("u{bit_size}");
    let enum_name = &input.ident;
    let bit_size_value: u8 = bit_size
        .base10_parse()
        .map_err(|e| e.into_compile_error().into_token_stream())?;

    for variant in &enum_data.variants {
        if !variant.fields.is_empty() {
            return Err(error!(
                variant.fields.span(),
                "Bit enums cannot have fields"
            ));
        }

        let Some((_, discriminant)) = &variant.discriminant else {
            return Err(error!(
                variant.span(),
                "Bit enum variants must have explicit discriminants"
            ));
        };

        let syn::Expr::Lit(discriminant) = discriminant else {
            return Err(error!(
                discriminant.span(),
                "Discriminants must be integer literals"
            ));
        };

        let syn::Lit::Int(discriminant) = &discriminant.lit else {
            return Err(error!(
                discriminant.span(),
                "Discriminants must be integer literals"
            ));
        };

        let discriminant_value: u128 = discriminant
            .base10_parse()
            .map_err(|e| e.into_compile_error().into_token_stream())?;

        if discriminant_value == 0 {
            return Err(error!(
                discriminant.span(),
                "Discriminants must not be zero"
            ));
        }

        if !discriminant_value.is_power_of_two() {
            return Err(error!(
                discriminant.span(),
                "Discriminants must be powers of two - a.k.a. contain a single bit set"
            ));
        }

        if discriminant_value.trailing_zeros() as u8 > bit_size_value - 1 {
            return Err(error!(
                discriminant.span(),
                format!("Discriminant size must be at most {bit_size} bits")
            ));
        }
    }

    let variants = enum_data.variants.iter().map(|v| {
        let name = &v.ident;
        quote! { #enum_name::#name }
    });

    let variants_clone = enum_data.variants.iter().map(|v| {
        let name = &v.ident;
        quote! { #enum_name::#name }
    });

    Ok(quote! {
        impl ::bity::BitSetElement for #enum_name
        {
            type Raw = #inner_ty;
            const SET_MASK: Self::Raw = {
                #inner_ty::new(#(#variants_clone as <#inner_ty as Number>::UnderlyingType)|*)
            };

            #[inline]
            fn variants() -> &'static [#enum_name] {
                &[
                    #(#variants),*
                ]
            }

            #[inline]
            fn set_insert(set: Self::Raw, value: Self) -> Self::Raw {
                set | (Self::Raw::new(value as <#inner_ty as Number>::UnderlyingType))
            }

            #[inline]
            fn set_remove(set: Self::Raw, value: Self) -> Self::Raw {
                set & !(Self::Raw::new(value as <#inner_ty as Number>::UnderlyingType))
            }

            #[inline]
            fn set_contains(set: Self::Raw, value: Self) -> bool {
                set.value() & (value as <#inner_ty as Number>::UnderlyingType) != 0
            }

            #[inline]
            fn set_len(set: Self::Raw) -> usize {
                (set & Self::SET_MASK).count_ones() as usize
            }

            #[inline]
            fn set_not(set: Self::Raw) -> Self::Raw {
                !set
            }

            #[inline]
            fn set_or(lhs: Self::Raw, rhs: Self::Raw) -> Self::Raw {
                lhs | rhs
            }

            #[inline]
            fn set_and(lhs: Self::Raw, rhs: Self::Raw) -> Self::Raw {
                lhs & rhs
            }
        }
    })
}
