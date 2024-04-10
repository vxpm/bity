use super::error;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{spanned::Spanned, Data, DeriveInput, LitInt};

pub fn generate(bit_size: LitInt, input: DeriveInput) -> Result<TokenStream, TokenStream> {
    let Data::Enum(enum_data) = input.data else {
        unreachable!()
    };

    let inner_ty_name = format_ident!("u{bit_size}");
    let inner_ty = quote! { ::bity::raw::#inner_ty_name };
    let enum_name = &input.ident;
    let bit_size_value: u8 = bit_size
        .base10_parse()
        .map_err(|e| e.into_compile_error().into_token_stream())?;

    for variant in &enum_data.variants {
        if !variant.fields.is_empty() {
            return Err(error!(variant.fields.span(), "Cannot have fields"));
        }

        let Some((_, discriminant)) = &variant.discriminant else {
            return Err(error!(variant.span(), "Must have explicit discriminants"));
        };

        let syn::Expr::Lit(discriminant) = discriminant else {
            return Err(error!(
                discriminant.span(),
                "Discriminant must be integer literals"
            ));
        };

        let syn::Lit::Int(discriminant) = &discriminant.lit else {
            return Err(error!(
                discriminant.span(),
                "Discriminant must be integer literals"
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

        if discriminant_value.trailing_zeros() as u8 > bit_size_value - 1 {
            return Err(error!(
                discriminant.span(),
                format!("Discriminant must be in the 1..={bit_size} range")
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
        impl ::bity::VariantSetEnum for #enum_name
        {
            const VARIANTS: &'static [Self] = {
                &[
                    #(#variants),*
                ]
            };

            const SET_MASK: Self::Raw = {
                #inner_ty::new(#((1 << (#variants_clone as <#inner_ty as Number>::UnderlyingType - 1)))|*)
            };

            #[inline]
            fn into_bit(self) -> Self::Raw {
                Self::Raw::new(1 << (self as <#inner_ty as Number>::UnderlyingType - 1))
            }
        }
    })
}
