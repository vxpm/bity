use super::error;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{spanned::Spanned, ItemEnum, LitInt};

pub fn generate(bit_size: LitInt, def: ItemEnum) -> Result<TokenStream, TokenStream> {
    let inner_ty = format_ident!("u{bit_size}");
    let enum_name = &def.ident;
    let bit_size_value: u8 = bit_size
        .base10_parse()
        .map_err(|e| e.into_compile_error().into_token_stream())?;
    let max_variant_count = 2u128.pow(bit_size_value as u32);

    let mut discriminant_to_variant = Vec::new();
    let mut variant_to_discriminant = Vec::new();
    for variant in &def.variants {
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

        if discriminant_value >= max_variant_count {
            return Err(error!(
                discriminant.span(),
                format!("Discriminant must be in the 0..{max_variant_count} range")
            ));
        }

        let variant_name = &variant.ident;
        discriminant_to_variant.push(quote! { x if x == { const DISCRIMINANT: #inner_ty = #inner_ty::new(#discriminant); DISCRIMINANT } => Some(Self::#variant_name) });
        variant_to_discriminant.push(quote! { Self::#variant_name => { const DISCRIMINANT: #inner_ty = #inner_ty::new(#discriminant); DISCRIMINANT } });
    }

    let is_full = discriminant_to_variant.len() as u128 == max_variant_count;
    let bits_impl = is_full.then(|| {
        quote! {
            #[allow(ambiguous_associated_items)]
            impl ::bity::Bits for #enum_name {
                #[inline(always)]
                fn from_raw(value: Self::Raw) -> Self {
                    unsafe { Self::try_from_raw(value).unwrap_unchecked() }
                }
            }

            impl From<#inner_ty> for #enum_name {
                #[inline(always)]
                fn from(value: #inner_ty) -> #enum_name {
                    Self::from_raw(value)
                }
            }
        }
    });

    let try_from_impl = (!is_full).then(|| {
        quote! {
            impl TryFrom<#inner_ty> for #enum_name {
                type Error = ();

                #[inline(always)]
                fn try_from(value: #inner_ty) -> Result<Self, ()> {
                    Self::try_from_raw(value).ok_or(())
                }
            }
        }
    });

    Ok(quote! {
        #[derive(Clone, Copy)]
        #def

        #[allow(ambiguous_associated_items)]
        impl ::bity::TryBits for #enum_name {
            type Raw = #inner_ty;

            #[inline(always)]
            fn try_from_raw(value: Self::Raw) -> Option<Self> {
                match value {
                    #(#discriminant_to_variant,)*
                    _ => None
                }
            }

            #[inline(always)]
            fn into_raw(self) -> Self::Raw {
                match self {
                    #(#variant_to_discriminant,)*
                }
            }
        }

        impl From<#enum_name> for #inner_ty {
            #[inline(always)]
            fn from(value: #enum_name) -> #inner_ty {
                value.into_raw()
            }
        }

        #try_from_impl
        #bits_impl
    })
}
