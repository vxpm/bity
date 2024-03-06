mod bity_enum;
mod bity_struct;

use syn::{parse_macro_input, spanned::Spanned, Item};

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
