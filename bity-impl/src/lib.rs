mod bity_enum;
mod bity_set;
mod bity_struct;

use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Item};

macro_rules! error {
    ($span:expr, $message:expr $(,)?) => {
        syn::Error::new($span, $message).to_compile_error().into()
    };
}
pub(crate) use error;

#[proc_macro_attribute]
pub fn bity(
    input: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let bit_size = parse_macro_input!(input);
    let item: Item = parse_macro_input!(item);

    let result = match item {
        Item::Struct(s) => bity_struct::generate(bit_size, s),
        Item::Enum(e) => bity_enum::generate(bit_size, e),
        _ => return error!(item.span(), "Unsupported Item"),
    };

    match result {
        Ok(x) => x,
        Err(e) => e,
    }
    .into()
}

#[proc_macro_derive(BitySet, attributes(bity_set))]
pub fn bity_set(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = parse_macro_input!(item);
    let Some(attr) = input
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("bity_set"))
        .nth(0)
    else {
        return error!(
            input.span(),
            "Missing bity_set attribute specifying bit size"
        );
    };

    let Ok(args) = attr.meta.require_list() else {
        return error!(
            attr.span(),
            "Attribute argument must be a single integer literal specifying bit size"
        );
    };

    let Ok(bit_size) = syn::parse2(args.tokens.clone()) else {
        return error!(
            args.span(),
            "Attribute argument must be a single integer literal specifying bit size"
        );
    };

    let result = match input.data {
        Data::Enum(_) => bity_set::generate(bit_size, input),
        _ => return error!(input.span(), "Unsupported Item"),
    };

    match result {
        Ok(x) => x,
        Err(e) => e,
    }
    .into()
}
