use super::error;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::parse_quote;
use syn::Attribute;
use syn::Expr;
use syn::ExprLit;
use syn::ExprRange;
use syn::Fields;
use syn::GenericArgument;
use syn::Ident;
use syn::Lit;
use syn::Path;
use syn::PathArguments;
use syn::PathSegment;
use syn::RangeLimits;
use syn::Type;
use syn::{spanned::Spanned, Field, ItemStruct, LitInt};

// thanks to https://stackoverflow.com/questions/55271857/how-can-i-get-the-t-from-an-optiont-when-using-syn
// i was too lazy. macro code is hell! i hate it!
fn extract_option_inner(ty: &syn::Type) -> Option<&syn::Type> {
    fn extract_ty_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            Type::Path(ref path) if path.qself.is_none() => Some(&path.path),
            _ => None,
        }
    }

    fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
        let path_idents = path
            .segments
            .iter()
            .into_iter()
            .fold(Vec::new(), |mut acc, v| {
                acc.push(&v.ident);
                acc
            });

        let recognized: Vec<&'static [&str]> = vec![
            &["Option"],
            &["std", "option", "Option"],
            &["core", "option", "Option"],
        ];

        recognized
            .into_iter()
            .find(|s| path_idents.iter().eq(s.iter()))
            .and_then(|_| path.segments.last())
    }

    extract_ty_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;

            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

fn extract_bit_range(bit_range: ExprRange, max: LitInt) -> Result<(LitInt, LitInt), TokenStream> {
    if let RangeLimits::Closed(_) = &bit_range.limits {
        return Err(error!(bit_range.span(), "Bit range must be half-open"));
    }

    let bit_range_start = bit_range
        .start
        .as_ref()
        .map(
            |boxed_bit_range_start| match (**boxed_bit_range_start).clone() {
                Expr::Lit(ExprLit {
                    lit: Lit::Int(bit_range_start),
                    ..
                }) => Ok(bit_range_start),
                _ => Result::<_, TokenStream>::Err(error!(
                    bit_range.span(),
                    "Invalid bit range start"
                )),
            },
        )
        .transpose()?;

    let bit_range_end = bit_range
        .end
        .as_ref()
        .map(
            |boxed_bit_range_end| match (**boxed_bit_range_end).clone() {
                Expr::Lit(ExprLit {
                    lit: Lit::Int(bit_range_end),
                    ..
                }) => Ok(bit_range_end),
                _ => {
                    Result::<_, TokenStream>::Err(error!(bit_range.span(), "Invalid bit range end"))
                }
            },
        )
        .transpose()?;

    Ok((
        bit_range_start.unwrap_or(parse_quote! { 0 }),
        bit_range_end.unwrap_or(max),
    ))
}

fn doc_comment_attrs(attrs: &[Attribute]) -> impl Iterator<Item = &Attribute> {
    let doc_comment_path = parse_quote! { doc };
    attrs.iter().filter(
        move |attr| matches!(&attr.meta, syn::Meta::NameValue(x) if x.path == doc_comment_path),
    )
}

fn method_prelude() -> TokenStream {
    quote! {
        use ::bity::Bits;
        use ::bity::prelude::Number;
        use ::bity::prelude::BitUtils;
    }
}

fn range_assertions(
    field_name: &Ident,
    field_bit_size: TokenStream,
    struct_bit_size: LitInt,
    (bit_range_start, bit_range_end): (LitInt, LitInt),
) -> TokenStream {
    let error_msg_size = format!("Bitrange of field '{field_name}' is not the correct size");
    let error_msg_start = format!(
        "Bitrange of field '{field_name}' must have a start in the 0..{struct_bit_size} range"
    );
    let error_msg_end = format!(
        "Bitrange of field '{field_name}' must have an end in the 0..={struct_bit_size} range"
    );

    quote! {
        const _: () = {
            let start: usize = #bit_range_start;
            let end: usize = #bit_range_end;
            assert!(start < #struct_bit_size, #error_msg_start);
            assert!(end <= #struct_bit_size, #error_msg_end);
            assert!((end).saturating_sub(start) == #field_bit_size, #error_msg_size);
        };
    }
}

fn simple_field_getter(
    field: &Field,
    (bit_range_start, bit_range_end): (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let field_ty = &field.ty;

    let doc_comment_attrs = doc_comment_attrs(&field.attrs);

    if let Some(inner_ty) = extract_option_inner(field_ty) {
        let raw_inner_ty = quote! { <#inner_ty as TryBits>::Raw  };
        let range_assertions = range_assertions(
            field_name,
            quote! { <#raw_inner_ty as Number>::BITS },
            bit_struct.bit_size.clone(),
            (bit_range_start.clone(), bit_range_end.clone()),
        );

        let method_prelude = method_prelude();
        Ok(quote! {
            #(#doc_comment_attrs)*
            #[inline]
            #field_vis fn #field_name (self) -> #field_ty {
                #method_prelude

                #range_assertions

                let extracted = unsafe { <#raw_inner_ty as Number>::UnderlyingType::try_from(self.__raw.value().bits(#bit_range_start, #bit_range_end)).unwrap_unchecked() };
                <#inner_ty as TryBits>::try_from_raw(<#raw_inner_ty as Number>::new(extracted))
            }
        })
    } else {
        let raw_field_ty = quote! { <#field_ty as TryBits>::Raw  };
        let range_assertions = range_assertions(
            field_name,
            quote! { <#raw_field_ty as Number>::BITS },
            bit_struct.bit_size.clone(),
            (bit_range_start.clone(), bit_range_end.clone()),
        );

        let method_prelude = method_prelude();
        Ok(quote! {
            #(#doc_comment_attrs)*
            #[inline]
            #field_vis fn #field_name (self) -> #field_ty {
                #method_prelude

                #range_assertions

                let extracted = unsafe { self.__raw.value().bits(#bit_range_start, #bit_range_end).try_into().unwrap_unchecked() };
                <#field_ty as Bits>::from_raw(<#raw_field_ty as Number>::new(extracted))
            }
        })
    }
}

fn simple_field_setter(
    field: &Field,
    (bit_range_start, bit_range_end): (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let bit_struct_inner_ty = &bit_struct.inner_ty;
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let field_ty = &field.ty;

    let setter_name = format_ident!("with_{field_name}");
    let setter_doc_string =
        format!("Sets the value of the `{field_name}` field and returns the modified value.");

    let method_prelude = method_prelude();
    if let Some(inner_ty) = extract_option_inner(field_ty) {
        Ok(quote! {
            #[doc = #setter_doc_string]
            #[inline]
            #[must_use]
            #field_vis fn #setter_name (self, value: #inner_ty) -> Self {
                #method_prelude

                let new_value = unsafe { value.into_raw().value().try_into().unwrap_unchecked() };
                let new_inner = self.__raw.value().with_bits(#bit_range_start, #bit_range_end, new_value);
                Self { __raw: <#bit_struct_inner_ty>::new(new_inner) }
            }
        })
    } else {
        Ok(quote! {
            #[doc = #setter_doc_string]
            #[inline]
            #[must_use]
            #field_vis fn #setter_name (self, value: #field_ty) -> Self {
                #method_prelude

                let new_value = unsafe { value.into_raw().value().try_into().unwrap_unchecked() };
                let new_inner = self.__raw.value().with_bits(#bit_range_start, #bit_range_end, new_value);
                Self { __raw: <#bit_struct_inner_ty>::new(new_inner) }
            }
        })
    }
}

fn simple_field_methods(
    field: &Field,
    bit_range: (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let getter = simple_field_getter(field, bit_range.clone(), bit_struct)?;
    let setter = simple_field_setter(field, bit_range, bit_struct)?;

    Ok(quote! {
        #getter
        #setter
    })
}

fn array_field_elem_getter(
    field: &Field,
    (bit_range_start, _): (LitInt, LitInt),
) -> Result<TokenStream, TokenStream> {
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let (field_elem_ty, elem_count) = {
        let Type::Array(arr) = &field.ty else {
            unreachable!("already checked before")
        };

        (&*arr.elem, &arr.len)
    };
    let raw_field_elem_ty = quote! { <#field_elem_ty as TryBits>::Raw  };

    let getter_name = format_ident!("{field_name}_at");
    let try_getter_name = format_ident!("try_{field_name}_at");

    let getter_doc_string =
        format!("Gets the value at the given index of the `{field_name}` field. Panics if the index is out of bounds.");
    let try_getter_doc_string =
        format!("Tries to get the value at the given index of the `{field_name}` field.");

    let method_prelude = method_prelude();
    Ok(quote! {
        #[doc = #getter_doc_string]
        #[inline]
        #field_vis fn #getter_name (self, index: usize) -> #field_elem_ty {
            #method_prelude

            if index >= #elem_count {
                panic!("Index out of bounds");
            }

            let elem_start = #bit_range_start + <#raw_field_elem_ty as Number>::BITS * index;
            let elem_end = elem_start + <#raw_field_elem_ty as Number>::BITS;

            let extracted = unsafe { self.__raw.value().bits(elem_start as u8, elem_end as u8).try_into().unwrap_unchecked() };
            <#field_elem_ty as Bits>::from_raw(<#raw_field_elem_ty as Number>::new(extracted))
        }

        #[doc = #try_getter_doc_string]
        #[inline]
        #field_vis fn #try_getter_name (self, index: usize) -> Option<#field_elem_ty> {
            #method_prelude

            (index < #elem_count).then(|| {
                let elem_start = #bit_range_start + <#raw_field_elem_ty as Number>::BITS * index;
                let elem_end = elem_start + <#raw_field_elem_ty as Number>::BITS;

                let extracted = unsafe { self.__raw.value().bits(elem_start as u8, elem_end as u8).try_into().unwrap_unchecked() };
                <#field_elem_ty as Bits>::from_raw(<#raw_field_elem_ty as Number>::new(extracted))
            })
        }
    })
}

fn array_field_elem_setter(
    field: &Field,
    (bit_range_start, _): (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let bit_struct_inner_ty = &bit_struct.inner_ty;
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let (field_elem_ty, elem_count) = {
        let Type::Array(arr) = &field.ty else {
            unreachable!("already checked before")
        };

        (&*arr.elem, &arr.len)
    };
    let raw_field_elem_ty = quote! { <#field_elem_ty as TryBits>::Raw  };

    let setter_name = format_ident!("with_{field_name}_at");
    let try_setter_name = format_ident!("try_with_{field_name}_at");

    let setter_doc_string =
        format!("Sets the value at the given index of the `{field_name}` field and returns the modified value. Panics if the index is out of bounds.");
    let try_setter_doc_string =
        format!("Tries to set the value at the given index of the `{field_name}` field and returns the modified value.");

    let method_prelude = method_prelude();
    Ok(quote! {
        #[doc = #setter_doc_string]
        #[inline]
        #[must_use]
        #field_vis fn #setter_name (self, index: usize, value: #field_elem_ty) -> Self {
            #method_prelude

            if index >= #elem_count {
                panic!("Index out of bounds");
            }

            let elem_start = #bit_range_start + <#raw_field_elem_ty as Number>::BITS * index;
            let elem_end = elem_start + <#raw_field_elem_ty as Number>::BITS;

            let new_value = unsafe { value.into_raw().value().try_into().unwrap_unchecked() };
            let new_inner = self.__raw.value().with_bits(elem_start as u8, elem_end as u8, new_value);
            Self { __raw: <#bit_struct_inner_ty>::new(new_inner) }
        }

        #[doc = #try_setter_doc_string]
        #[inline]
        #[must_use]
        #field_vis fn #try_setter_name (self, index: usize, value: #field_elem_ty) -> Option<Self> {
            #method_prelude

            (index < #elem_count).then(|| {
                let elem_start = #bit_range_start + <#raw_field_elem_ty as Number>::BITS * index;
                let elem_end = elem_start + <#raw_field_elem_ty as Number>::BITS;

                let new_value = unsafe { value.into_raw().value().try_into().unwrap_unchecked() };
                let new_inner = self.__raw.value().with_bits(elem_start as u8, elem_end as u8, new_value);
                Self { __raw: <#bit_struct_inner_ty>::new(new_inner) }
            })
        }
    })
}

fn array_field_getter(
    field: &Field,
    (bit_range_start, bit_range_end): (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let (field_elem_ty, elem_count) = {
        let Type::Array(arr) = &field.ty else {
            unreachable!("already checked before")
        };

        (&*arr.elem, &arr.len)
    };
    let field_ty = &field.ty;

    let raw_field_elem_ty = quote! { <#field_elem_ty as TryBits>::Raw };
    let range_assertions = range_assertions(
        field_name,
        quote! { <#raw_field_elem_ty as Number>::BITS * #elem_count },
        bit_struct.bit_size.clone(),
        (bit_range_start.clone(), bit_range_end.clone()),
    );

    let elem_getter_name = format_ident!("{field_name}_at");
    let getter_name = format_ident!("{field_name}");

    let doc_comment_attrs = doc_comment_attrs(&field.attrs);

    let method_prelude = method_prelude();
    Ok(quote! {
        #(#doc_comment_attrs)*
        #[inline]
        #field_vis fn #getter_name (self) -> #field_ty {
            #method_prelude

            #range_assertions

            std::array::from_fn(|index| self.#elem_getter_name(index))
        }
    })
}

fn array_field_setter(field: &Field) -> Result<TokenStream, TokenStream> {
    let field_vis = &field.vis;
    let field_name = field.ident.as_ref().expect("named fields only");
    let field_ty = &field.ty;

    let elem_setter_name = format_ident!("with_{field_name}_at");
    let setter_name = format_ident!("with_{field_name}");

    let setter_doc_string = format!("Sets the value of the `{field_name}` field.");

    let method_prelude = method_prelude();
    Ok(quote! {
        #[doc = #setter_doc_string]
        #[inline]
        #[must_use]
        #field_vis fn #setter_name (self, value: #field_ty) -> Self {
            #method_prelude

            let mut result = self;
            for (index, elem) in value.into_iter().enumerate() {
                result = result.#elem_setter_name(index, elem);
            }

            result
        }
    })
}

fn array_field_methods(
    field: &Field,
    bit_range: (LitInt, LitInt),
    bit_struct: &BitStruct,
) -> Result<TokenStream, TokenStream> {
    let elem_getter = array_field_elem_getter(field, bit_range.clone())?;
    let elem_setter = array_field_elem_setter(field, bit_range.clone(), bit_struct)?;
    let getter = array_field_getter(field, bit_range.clone(), bit_struct)?;
    let setter = array_field_setter(field)?;

    Ok(quote! {
        #getter
        #setter
        #elem_getter
        #elem_setter
    })
}

fn field_methods(field: &Field, bit_struct: &BitStruct) -> Result<TokenStream, TokenStream> {
    let Some(bits) = field.attrs.iter().find(|attr| attr.path().is_ident("bits")) else {
        return Err(error!(field.span(), "Field needs a #[bits(..)] attribute"));
    };

    let bit_range_expr: ExprRange = bits.parse_args().map_err(|e| e.into_compile_error())?;
    let bit_range = extract_bit_range(bit_range_expr, bit_struct.bit_size.clone())?;

    match field.ty {
        Type::Array(_) => array_field_methods(field, bit_range, bit_struct),
        _ => simple_field_methods(field, bit_range, bit_struct),
    }
}

struct BitStruct {
    pub original_def: ItemStruct,
    pub def: ItemStruct,
    pub bit_size: LitInt,
    pub inner_ty: Type,
    pub generate_debug: bool,
}

impl BitStruct {
    pub fn new(original_def: ItemStruct, bit_size: LitInt) -> Result<Self, TokenStream> {
        let inner_ty = {
            let inner_ty_name = format_ident!("u{bit_size}");
            let inner_ty = quote! { ::bity::raw::#inner_ty_name };
            parse_quote! { #inner_ty }
        };

        let ItemStruct {
            attrs,
            vis,
            ident,
            generics,
            fields,
            ..
        } = &original_def;

        if generics.type_params().count() != 0 {
            return Err(error!(
                generics.span(),
                "Bit structs cannot be generic over types"
            ));
        }

        if !matches!(fields, Fields::Named(_)) {
            return Err(error!(fields.span(), "Bit structs must have named fields"));
        }

        let mut new_attrs = attrs.clone();
        let generate_debug = new_attrs.iter_mut().any(|attr| {
            let syn::Meta::List(list) = &mut attr.meta else {
                return false;
            };

            if !list.path.is_ident("derive") {
                return false;
            }

            let parser =
                syn::punctuated::Punctuated::<syn::Path, syn::token::Comma>::parse_terminated;
            let Ok(mut args) = list.parse_args_with(parser) else {
                return false;
            };

            let debug_index = args
                .iter()
                .enumerate()
                .find_map(|(index, arg)| arg.is_ident("Debug").then_some(index));

            let Some(debug_index) = debug_index else {
                return false;
            };

            let inverted_debug_index = args.len() - debug_index - 1;
            let mut new_args = syn::punctuated::Punctuated::<syn::Path, syn::token::Comma>::new();
            for i in 0..args.len() {
                let arg = args.pop().unwrap().into_value();
                if i != inverted_debug_index {
                    new_args.push(arg);
                }
            }

            list.tokens = new_args.to_token_stream();
            true
        });

        let (impl_generics, _, where_clause) = generics.split_for_impl();
        Ok(Self {
            def: parse_quote! {
                #(#new_attrs)*
                #[derive(Clone, Copy)]
                #vis struct #ident #impl_generics #where_clause {
                    /// This is the raw, inner value of this struct. You can directly manipulate it
                    /// if desired, but prefer using `into_raw` and `(try_)from_raw`.
                    __raw: #inner_ty
                }
            },
            original_def,
            bit_size,
            inner_ty,
            generate_debug,
        })
    }
}

pub fn generate(
    struct_bit_size: LitInt,
    struct_def: ItemStruct,
) -> Result<TokenStream, TokenStream> {
    let bit_struct = BitStruct::new(struct_def, struct_bit_size)?;
    let bit_struct_def = &bit_struct.def;
    let bit_struct_name = &bit_struct.def.ident;
    let bit_struct_name_str = &bit_struct.def.ident.to_string();
    let bit_struct_inner_ty = &bit_struct.inner_ty;

    let field_names = bit_struct
        .original_def
        .fields
        .iter()
        .map(|f| f.ident.as_ref().expect("only named fields"));
    let field_names_str = bit_struct
        .original_def
        .fields
        .iter()
        .map(|f| f.ident.as_ref().expect("only named fields").to_string());
    let fields_methods: Result<Vec<TokenStream>, TokenStream> = bit_struct
        .original_def
        .fields
        .iter()
        .map(|f| field_methods(f, &bit_struct))
        .collect();
    let fields_methods = fields_methods?;

    let (impl_generics, ty_generics, where_clause) = bit_struct_def.generics.split_for_impl();
    let debug_impl = if bit_struct.generate_debug {
        Some(quote! {
            impl #impl_generics ::core::fmt::Debug for #bit_struct_name #ty_generics #where_clause {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    f.debug_struct(#bit_struct_name_str)
                        #(.field(#field_names_str, &self.#field_names()))*
                        .finish()
                }
            }
        })
    } else {
        None
    };

    Ok(quote! {
        #bit_struct_def

        impl #impl_generics #bit_struct_name #ty_generics #where_clause {
            #(#fields_methods)*
        }

        #debug_impl

        impl #impl_generics From<#bit_struct_name #ty_generics> for #bit_struct_inner_ty #where_clause {
            #[inline(always)]
            fn from(value: #bit_struct_name #ty_generics) -> #bit_struct_inner_ty {
                value.__raw
            }
        }

        impl #impl_generics ::bity::TryBits for #bit_struct_name #ty_generics #where_clause {
            type Raw = #bit_struct_inner_ty;

            #[inline(always)]
            fn try_from_raw(value: Self::Raw) -> Option<Self> {
                Some(Self { __raw: value })
            }

            #[inline(always)]
            fn into_raw(self) -> Self::Raw {
                self.__raw
            }
        }

        impl #impl_generics ::bity::Bits for #bit_struct_name #ty_generics #where_clause {
            #[inline(always)]
            fn from_raw(value: Self::Raw) -> Self {
                Self { __raw: value }
            }
        }

        impl #impl_generics From<#bit_struct_inner_ty> for #bit_struct_name #ty_generics #where_clause {
            #[inline(always)]
            fn from(value: #bit_struct_inner_ty) -> Self {
                Self { __raw: value }
            }
        }
    })
}
