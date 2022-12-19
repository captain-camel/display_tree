mod attrs;
mod gen;

/// Derive marco for the `DislayTree` trait.
///
/// See the `DisplayTree` documentation for more information.
#[proc_macro_derive(
    DisplayTree,
    attributes(field_label, ignore_field, node_label, optional_field, tree)
)]
#[proc_macro_error::proc_macro_error]
pub fn derive_display_tree(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(tokens)
        .expect("rust should ensure `derive` is only applied to types");
    gen::generate(ast).into()
}
