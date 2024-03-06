use super::error;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{spanned::Spanned, ItemEnum, LitInt};

pub fn generate(bit_size: LitInt, def: ItemEnum) -> Result<TokenStream, TokenStream> {
    let inner_ty = format_ident!("u{bit_size}");
    let enum_name = &def.ident;
    let bit_size_num = bit_size
        .base10_parse()
        .map_err(|e| e.into_compile_error().into_token_stream())?;
    let variant_count = 2usize.pow(bit_size_num);

    let mut variants = Vec::new();
    let mut variants_assertions = Vec::new();
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

        let variant_name = &variant.ident;
        let panic_msg = format!(
            "Discriminant of variant `{variant_name}` must be in the 0..{variant_count} range"
        );
        variants_assertions.push(quote! {
            const _: () = {
                assert!(#discriminant < #variant_count, #panic_msg);
            };
        });
        variants.push(quote! { x if x == { const DISCRIMINANT: #inner_ty = #inner_ty::new(#discriminant); DISCRIMINANT } => Some(Self::#variant_name) });
    }

    let bits_impl = (variants.len() == variant_count).then(|| {
        quote! {
            impl ::bity::Bits for #enum_name {
                #[inline(always)]
                fn from_raw(value: Self::Raw) -> Self {
                    unsafe { Self::try_from_raw(value).unwrap_unchecked() }
                }
            }
        }
    });

    Ok(quote! {
        #def

        impl ::bity::TryBits for #enum_name {
            type Raw = #inner_ty;

            fn try_from_raw(value: Self::Raw) -> Option<Self> {
                #(#variants_assertions)*

                match value {
                    #(#variants,)*
                    _ => None
                }
            }

            fn into_raw(self) -> Self::Raw {
                unsafe { (self as u128).try_into().unwrap_unchecked() }
            }
        }

        #bits_impl
    })
}
