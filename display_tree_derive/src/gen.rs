use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, abort_call_site};
use quote::{quote, quote_spanned, TokenStreamExt};
use syn::spanned::Spanned;

use super::attrs;

struct ConstIdent(&'static str);

impl quote::ToTokens for ConstIdent {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append(syn::Ident::new(self.0, proc_macro2::Span::call_site()))
    }
}

const FORMATTER_PARAM: ConstIdent = ConstIdent("__display_tree_f");
const STYLE_PARAM: ConstIdent = ConstIdent("__display_tree_style");

pub fn generate(ast: syn::DeriveInput) -> TokenStream2 {
    let fn_inner = match ast.data {
        syn::Data::Struct(data) => {
            let config = match attrs::node::Config::from(&ast.attrs) {
                Ok(config) => config,
                Err(error) => abort!(error.span, "{}", error),
            };
            generate_struct_field_writes(&ast.ident, data.fields, config)
        }
        syn::Data::Enum(data) => {
            let arms = data.variants.into_iter().map(generate_arm);
            quote! {
                match self {
                    #(
                        #arms
                    ),*
                }
            }
        }
        _ => abort_call_site!("`DisplayTree` can only be derived for `struct`s and `enum`s"),
    };

    let ty = ast.ident;
    let generics = ast.generics;

    quote! {
        impl #generics ::display_tree::DisplayTree for #ty #generics {
            fn fmt(&self, #FORMATTER_PARAM: &mut std::fmt::Formatter, #STYLE_PARAM: ::display_tree::Style) -> ::std::fmt::Result {
                #fn_inner
                Ok(())
            }
        }
    }
}

pub fn generate_struct_field_writes<'a>(
    ident: &syn::Ident,
    fields: syn::Fields,
    struct_config: attrs::node::Config,
) -> TokenStream2 {
    let mut node_label_member_expression: Option<TokenStream2> = None;

    let mut leaf_expressions: Vec<TokenStream2> = Vec::new();
    for (index, field) in fields.iter().enumerate() {
        let field_member_expression = if let Some(ident) = &field.ident {
            quote! { self.#ident }
        } else {
            let index = syn::Index::from(index);
            quote! { self.#index }
        };

        let field_config = match attrs::field::Config::from(&field.attrs) {
            Ok(config) => config,
            Err(error) => abort!(error.span, "{}", error),
        };

        if field_config.is_node_label {
            if let Some(label) = struct_config.node_label {
                abort! { field.span(),
                    "conflicting helper attributes: cannot apply `{}` to field because variant `{}` already has a label specified",
                    attrs::field::NODE_LABEL,
                    ident;
                        note = "variant `{}`  has label \"{}\"", ident, label;
                }
            }

            node_label_member_expression = Some(field_member_expression.clone());

            // If the field is marked as a tree, it should still be written in adition to
            // being the node label.
            if !field_config.is_tree {
                continue;
            }
        }
        if field_config.is_ignored {
            continue;
        }

        use attrs::field::FieldLabel;
        let leaf_expression_format = match field_config.field_label {
            FieldLabel::Custom(label) => format!("{label}: {{}}"),
            FieldLabel::Default => {
                if let Some(identifier) = &field.ident {
                    format!("{}: {{}}", identifier.to_string())
                } else {
                    abort! { field.span(),
                        "cannot apply `{}` attribute to field because it is unnamed",
                        attrs::field::FIELD_LABEL,
                    }
                }
            }
            FieldLabel::None => format!("{{}}"),
        };

        let leaf_expression = if field_config.is_tree {
            // If the field is a tree, display it as one.
            // Use the span of the field so that errors show in the correct location.
            quote_spanned! { field.span() =>
                ::std::format!(
                    #leaf_expression_format,
                    ::display_tree::AsTree::with_style(
                        ::display_tree::to_display_tree_ref::ToDisplayTreeRef::to_display_tree(&#field_member_expression),
                        #STYLE_PARAM
                    )
                )
            }
        } else {
            // Otherwise try to use `Display`.
            // Use the span of the field so that errors show in the correct location.
            quote_spanned! { field.span() =>
                #STYLE_PARAM.leaf_style.apply(&::std::format!(#leaf_expression_format, #field_member_expression))
            }
        };

        leaf_expressions.push(leaf_expression);
    }

    let node_label_string_ref = if let Some(expression) = node_label_member_expression {
        // One of the fields of the variant should be used as the label.
        quote! {
            &format!("{}", #expression)
        }
    } else if let Some(label) = struct_config.node_label {
        // The variant has a custom label.
        quote! {
            #label
        }
    } else {
        // If not specified, the node label should be a string containing the name of
        // the variant.
        let variant_string = ident.to_string();
        quote! {
            #variant_string
        }
    };

    generate_field_writes(leaf_expressions, node_label_string_ref)
}

pub fn generate_arm(variant: syn::Variant) -> TokenStream2 {
    let config = match attrs::node::Config::from(&variant.attrs) {
        Ok(config) => config,
        Err(error) => abort!(error.span, "{}", error),
    };

    let variant_ident = &variant.ident;

    match &variant.fields {
        syn::Fields::Named(fields) => {
            let field_bindings = fields.named.iter().map(|field| {
                field
                    .ident
                    .as_ref()
                    .expect("named fields should have identifiers.")
            });

            let arm_inner = generate_enum_field_writes(&variant, field_bindings.clone(), config);

            quote! {
                Self::#variant_ident { #(#field_bindings),* } => {
                    #arm_inner
                }
            }
        }

        syn::Fields::Unnamed(fields) => {
            let field_bindings = (0..fields.unnamed.len())
                .map(|index| quote::format_ident!("_{}", index))
                .collect::<Vec<_>>();

            let arm_inner = generate_enum_field_writes(&variant, field_bindings.iter(), config);

            quote! {
                Self::#variant_ident(#(#field_bindings),*) => {
                    #arm_inner
                }
            }
        }

        syn::Fields::Unit => {
            let arm_inner = generate_enum_field_writes(&variant, std::iter::empty(), config);

            quote! {
                Self::#variant_ident => {
                    #arm_inner
                }
            }
        }
    }
}

pub fn generate_enum_field_writes<'a>(
    variant: &syn::Variant,
    field_bindings: impl Iterator<Item = &'a syn::Ident>,
    variant_config: attrs::node::Config,
) -> TokenStream2 {
    let mut node_label_binding: Option<&syn::Ident> = None;

    let mut leaf_expressions: Vec<TokenStream2> = Vec::new();
    for (field, binding) in std::iter::zip(variant.fields.iter(), field_bindings) {
        let field_config = match attrs::field::Config::from(&field.attrs) {
            Ok(config) => config,
            Err(error) => abort!(error.span, "{}", error),
        };

        if field_config.is_node_label {
            if let Some(label) = variant_config.node_label {
                abort! { field.span(),
                    "conflicting helper attributes: cannot apply `{}` attribute to field because variant `{}` already has a label specified",
                    attrs::field::NODE_LABEL,
                    variant.ident;
                        note = "variant `{}` has label \"{}\"", variant.ident, label;
                }
            }

            node_label_binding = Some(&binding);

            // If the field is marked as a tree, it should still be written in adition to
            // being the node label.
            if !field_config.is_tree {
                continue;
            }
        }
        if field_config.is_ignored {
            continue;
        }

        use attrs::field::FieldLabel;
        let leaf_expression_format = match field_config.field_label {
            FieldLabel::Custom(label) => format!("{label}: {{}}"),
            FieldLabel::Default => {
                if let Some(identifier) = &field.ident {
                    format!("{}: {{}}", identifier.to_string())
                } else {
                    abort! { field.span(),
                        "cannot apply `{}` attribute to field because it is unnamed",
                        attrs::field::FIELD_LABEL,
                    }
                }
            }
            FieldLabel::None => format!("{{}}"),
        };

        let leaf_expression = if field_config.is_tree {
            // If the field is a tree, display it as one.
            // Use the span of the field so that errors show in the correct location.
            quote_spanned! { field.span() =>
                ::std::format!(
                    #leaf_expression_format,
                    ::display_tree::AsTree::with_style(
                        ::display_tree::to_display_tree_ref::ToDisplayTreeRef::to_display_tree(#binding),
                        #STYLE_PARAM
                    )
                )
            }
        } else {
            // Otherwise try to use `Display`.
            // Use the span of the field so that errors show in the correct location.
            quote_spanned! { field.span() =>
                #STYLE_PARAM.leaf_style.apply(&::std::format!(#leaf_expression_format, #binding))
            }
        };

        leaf_expressions.push(leaf_expression);
    }

    let node_label_string_ref = if let Some(binding) = node_label_binding {
        // One of the fields of the variant should be used as the label.
        quote! {
            &format!("{}", #binding)
        }
    } else if let Some(label) = variant_config.node_label {
        // The variant has a custom label.
        quote! {
            #label
        }
    } else {
        // If not specified, the node label should be a string containing the name of
        // the variant.
        let variant_string = variant.ident.to_string();
        quote! {
            #variant_string
        }
    };

    generate_field_writes(leaf_expressions, node_label_string_ref)
}

fn generate_field_writes(
    leaf_expressions: Vec<TokenStream2>,
    node_label_string_ref: TokenStream2,
) -> TokenStream2 {
    let mut field_writes: Vec<TokenStream2> = Vec::with_capacity(leaf_expressions.len());
    for (index, leaf_expression) in leaf_expressions.iter().enumerate() {
        let leaf_first_line_connectors_string = if index == leaf_expressions.len() - 1 {
            quote! {
                ::std::format!(
                    "{}{} ",
                    #STYLE_PARAM.char_set.end_connector,
                    std::iter::repeat(#STYLE_PARAM.char_set.horizontal)
                        .take(#STYLE_PARAM.indentation as usize)
                        .collect::<String>()
                )
            }
        } else {
            quote! {
                ::std::format!(
                    "{}{} ",
                    #STYLE_PARAM.char_set.connector,
                    std::iter::repeat(#STYLE_PARAM.char_set.horizontal)
                        .take(#STYLE_PARAM.indentation as usize)
                        .collect::<String>()
                )
            }
        };

        let leaf_other_lines_connectors_string = if index == leaf_expressions.len() - 1 {
            quote! {
                ::std::format!(
                    " {} ",
                    std::iter::repeat(' ')
                        .take(#STYLE_PARAM.indentation as usize)
                        .collect::<String>()
                )
            }
        } else {
            quote! {
                ::std::format!(
                    "{}{} ",
                    #STYLE_PARAM.char_set.vertical,
                    std::iter::repeat(' ')
                        .take(#STYLE_PARAM.indentation as usize)
                        .collect::<String>()
                )
            }
        };

        field_writes.push(quote! {
            let s = #leaf_expression;
            let mut lines = s.lines();

            write!(
                #FORMATTER_PARAM,
                "\n{}{}",
                #STYLE_PARAM.branch_style.apply(&#leaf_first_line_connectors_string),
                lines.next().unwrap_or_default(),
            )?;

            for line in lines {
                write!(
                    #FORMATTER_PARAM,
                    "\n{}{}",
                    #STYLE_PARAM.branch_style.apply(&#leaf_other_lines_connectors_string),
                    line,
                )?;
            }
        })
    }

    quote! {
        write!(#FORMATTER_PARAM, "{}", #STYLE_PARAM.leaf_style.apply(#node_label_string_ref))?;
        #(#field_writes)*
    }
}
