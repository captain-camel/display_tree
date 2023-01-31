use proc_macro2::Span;

pub mod node {
    use syn::spanned::Spanned;

    use super::{AttributeParseError, AttributeParseErrorKind};

    pub const NODE_LABEL: &str = "node_label";

    pub enum Attribute {
        NodeLabel(String),
    }

    impl Attribute {
        pub fn parse(attribute: &syn::Attribute) -> Option<Result<Self, AttributeParseError>> {
            // FIXME: This should probably be parsed manually, currently it will miss
            // recognized attributes that are in the wrong format.
            let meta = attribute.parse_meta().ok()?;

            match meta.path().get_ident()?.to_string().as_str() {
                NODE_LABEL => {
                    if let syn::Meta::NameValue(syn::MetaNameValue { lit: literal, .. }) = meta {
                        if let syn::Lit::Str(label) = literal {
                            Some(Ok(Self::NodeLabel(label.value())))
                        } else {
                            Some(Err(AttributeParseError::new(
                                AttributeParseErrorKind::ExpectedStringLiteral(NODE_LABEL),
                                attribute.span(),
                            )))
                        }
                    } else {
                        Some(Err(AttributeParseError::new(
                            AttributeParseErrorKind::ExpectedEqual(NODE_LABEL),
                            attribute.span(),
                        )))
                    }
                }
                _ => None,
            }
        }
    }

    pub struct Config {
        pub node_label: Option<String>,
    }

    impl Config {
        pub fn default() -> Self {
            Self { node_label: None }
        }
    }

    impl Config {
        pub fn from(attributes: &[syn::Attribute]) -> Result<Self, AttributeParseError> {
            attributes.iter().flat_map(Attribute::parse).try_fold(
                Self::default(),
                |config, attribute| {
                    Ok(match attribute? {
                        Attribute::NodeLabel(label) => Self {
                            node_label: Some(label),
                            ..config
                        },
                    })
                },
            )
        }
    }
}

pub mod field {
    use syn::spanned::Spanned;

    use super::{AttributeParseError, AttributeParseErrorKind};

    pub const FIELD_LABEL: &str = "field_label";
    pub const IGNORE_FIELD: &str = "ignore_field";
    // TODO: rename to root_label
    pub const NODE_LABEL: &str = "node_label";
    pub const TREE: &str = "tree";
    // TODO: Add transparent attribute to signal that everthing should be at the
    // preious level. Then i can implement DisplayTree for vec and option

    pub enum Attribute {
        FieldLabel(Option<String>),
        IgnoreField,
        NodeLabel,
        Tree,
    }

    impl Attribute {
        pub fn parse(attribute: &syn::Attribute) -> Option<Result<Self, AttributeParseError>> {
            // FIXME: This should probably be parsed manually, currently it will miss
            // recognized attributes that are in the wrong format.
            let meta = attribute.parse_meta().ok()?;

            Some(match meta.path().get_ident()?.to_string().as_str() {
                FIELD_LABEL => match meta {
                    syn::Meta::Path(_) => Ok(Self::FieldLabel(None)),
                    syn::Meta::NameValue(syn::MetaNameValue { lit: literal, .. }) => {
                        if let syn::Lit::Str(label) = literal {
                            Ok(Self::FieldLabel(Some(label.value())))
                        } else {
                            Err(AttributeParseError::new(
                                AttributeParseErrorKind::ExpectedStringLiteral(FIELD_LABEL),
                                attribute.span(),
                            ))
                        }
                    }
                    _ => Err(AttributeParseError::new(
                        AttributeParseErrorKind::Malformed(FIELD_LABEL),
                        attribute.span(),
                    )),
                },
                IGNORE_FIELD => {
                    // The `ignore_field` attribute should only be a path, no arguments.
                    if matches!(meta, syn::Meta::Path(_)) {
                        Ok(Self::IgnoreField)
                    } else {
                        Err(AttributeParseError::new(
                            AttributeParseErrorKind::UnexpectedArgument(IGNORE_FIELD),
                            attribute.span(),
                        ))
                    }
                }
                NODE_LABEL => {
                    // The `node_label` attribute should only be a path, no arguments.
                    if matches!(meta, syn::Meta::Path(_)) {
                        Ok(Self::NodeLabel)
                    } else {
                        Err(AttributeParseError::new(
                            AttributeParseErrorKind::UnexpectedArgument(NODE_LABEL),
                            attribute.span(),
                        ))
                    }
                }
                TREE => {
                    // The `tree` attribute should only be a path, no arguments.
                    if matches!(meta, syn::Meta::Path(_)) {
                        Ok(Self::Tree)
                    } else {
                        Err(AttributeParseError::new(
                            AttributeParseErrorKind::UnexpectedArgument(TREE),
                            attribute.span(),
                        ))
                    }
                }
                _ => return None,
            })
        }
    }

    pub struct Config {
        pub field_label: FieldLabel,
        pub is_ignored: bool,
        pub is_node_label: bool,
        pub is_tree: bool,
    }

    pub enum FieldLabel {
        None,
        Default,
        Custom(String),
    }

    impl Config {
        pub fn default() -> Self {
            Self {
                field_label: FieldLabel::None,
                is_ignored: false,
                is_node_label: false,
                is_tree: false,
            }
        }
    }

    impl Config {
        pub fn from(attributes: &[syn::Attribute]) -> Result<Self, AttributeParseError> {
            let config = attributes.iter().flat_map(Attribute::parse).try_fold(
                Self::default(),
                |config, attribute| {
                    Ok(match attribute? {
                        Attribute::FieldLabel(label) => Self {
                            field_label: match label {
                                Some(label) => FieldLabel::Custom(label),
                                None => FieldLabel::Default,
                            },
                            ..config
                        },
                        Attribute::IgnoreField => Self {
                            is_ignored: true,
                            ..config
                        },
                        Attribute::NodeLabel => Self {
                            is_node_label: true,
                            ..config
                        },
                        Attribute::Tree => Self {
                            is_tree: true,
                            ..config
                        },
                    })
                },
            )?;

            if config.is_node_label && config.is_ignored {
                return Err(AttributeParseError::new(
                    AttributeParseErrorKind::Confict(NODE_LABEL, IGNORE_FIELD),
                    attributes.first().span(),
                ));
            }
            if config.is_ignored && config.is_tree {
                return Err(AttributeParseError::new(
                    AttributeParseErrorKind::Confict(IGNORE_FIELD, TREE),
                    attributes.first().span(),
                ));
            }

            Ok(config)
        }
    }
}

pub struct AttributeParseError {
    kind: AttributeParseErrorKind,
    pub span: Span,
}

pub enum AttributeParseErrorKind {
    Confict(&'static str, &'static str),
    ExpectedEqual(&'static str),
    ExpectedStringLiteral(&'static str),
    Malformed(&'static str),
    UnexpectedArgument(&'static str),
}

impl AttributeParseError {
    fn new(kind: AttributeParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl std::fmt::Display for AttributeParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self.kind {
            AttributeParseErrorKind::Confict(first, second) => {
                format!("conflicting helper attributes: cannot apply `{first}` and `{second}` to the same field")
            }
            AttributeParseErrorKind::ExpectedEqual(attribute) => {
                format!("expected `=`: `{attribute}` attribute requires an argument")
            }
            AttributeParseErrorKind::ExpectedStringLiteral(attribute) => {
                format!("expected string literal in `{attribute}` attribute")
            }
            AttributeParseErrorKind::Malformed(attribute) => {
                format!("malformed `{attribute} attribute`")
            }
            AttributeParseErrorKind::UnexpectedArgument(attribute) => {
                format!("`{attribute}` attribute takes no arguments")
            }
        };

        write!(f, "{string}")
    }
}
