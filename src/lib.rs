//! [`display_tree`] provides simple, automatic, and customizable tree
//! pretty-printing.
//!
//! This crate provies the [`DisplayTree`] trait and a macro to derive it for
//! `struct`s and `enum`s. The derived implementation can be heavily customized
//! using helper attributes discussed in the [`DisplayTree`
//! documentation](DisplayTree) It also provides [`AsTree`] and a set of macros
//! mirroring standard library counterparts for displaying or formatting tree
//! types. The way a tree is formatted can be customized with the [`Style`]
//! type, or builder methods on [`AsTree`], using the [`StyleBuilder`] trait.
//!
//! See the [`DisplayTree` documentation](DisplayTree) to learn how to make a
//! type printable as a tree, or [`AsTree`] or any of the [macros] provided by
//! [`display_tree`] for information on displaying or formatting your new tree
//! type.
//!
//! [macros]: https://docs.rs/display_tree/*/display_tree/#macros
//!
//! # Examples
//!
//! ```
//! use display_tree::{format_tree, CharSet, DisplayTree, Style, StyleBuilder};
//!
//! // A tree representing a numerical expression.
//! #[derive(DisplayTree)]
//! enum Expr {
//!     Int(i32),
//!     BinOp {
//!         #[node_label]
//!         op: char,
//!         #[tree]
//!         left: Box<Self>,
//!         #[tree]
//!         right: Box<Self>,
//!     },
//!     UnaryOp {
//!         #[node_label]
//!         op: char,
//!         #[tree]
//!         arg: Box<Self>,
//!     },
//! }
//!
//! let expr: Expr = Expr::BinOp {
//!     op: '+',
//!     left: Box::new(Expr::UnaryOp {
//!         op: '-',
//!         arg: Box::new(Expr::Int(2)),
//!     }),
//!     right: Box::new(Expr::Int(7)),
//! };
//!
//! assert_eq!(
//!     format_tree!(
//!         expr,
//!         Style::default()
//!             .indentation(1)
//!             .char_set(CharSet::DOUBLE_LINE)
//!     ),
//!     concat!(
//!         "+\n",
//!         "╠═ -\n",
//!         "║  ╚═ Int\n",
//!         "║     ╚═ 2\n",
//!         "╚═ Int\n",
//!         "   ╚═ 7",
//!     ),
//! );
//! ```

#![warn(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

extern crate self as display_tree;

mod display;
mod to_display_tree_ref;

use std::borrow::Cow;
use std::fmt;

pub use display::*;
pub use display_tree_derive::DisplayTree;

/// A type that can be pretty-printed as a tree with a specified style.
///
/// [`DisplayTree`] can be derived for `struct`s and `enum`s, and generally the
/// derived implementation should be sufficient, but it can be manually
/// implemented if needed.
///
/// # Deriving
///
/// Deriving [`DisplayTree`] for a type requires that all of its fields not
/// marked `#[tree]` implement [`std::fmt::Display`]. A derived implementation
/// will be formatted as the name of the `struct` or variant as a node, followed
/// by a branches underneath with a node for each field.
/// ```
/// use display_tree::{format_tree, AsTree, DisplayTree};
///
/// // A tree representing a numerical expression.
/// #[derive(DisplayTree)]
/// enum Expr {
///     Int(i32),
///     BinOp {
///         #[node_label]
///         op: char,
///         #[tree]
///         left: Box<Self>,
///         #[tree]
///         right: Box<Self>,
///     },
///     UnaryOp {
///         #[node_label]
///         op: char,
///         #[tree]
///         arg: Box<Self>,
///     },
/// }
///
/// # fn get_expr() -> Expr {
/// #   Expr::BinOp {
/// #       op: '+',
/// #       left: Box::new(Expr::UnaryOp {
/// #           op: '-',
/// #           arg: Box::new(Expr::Int(2))
/// #       }),
/// #       right: Box::new(Expr::Int(7))
/// #   }
/// # }
/// let expr: Expr = get_expr();
///
/// assert_eq!(
///     format_tree!(expr),
///     concat!(
///         "+\n",
///         "├── -\n",
///         "│   └── Int\n",
///         "│       └── 2\n",
///         "└── Int\n",
///         "    └── 7",
///     ),
/// );
/// ```
///
/// ## Helper Attributes
///
/// `derive(DisplayTree)` provies a few helper attribute that allow the
/// derived implementation to be customized.
///
/// ### Field Attributes
///
/// - `#[tree]` marks a field that should be formatted as a tree. By default, a
///   field's [`std::fmt::Display`] implementation will be used to format it in
///   the tree, but fields can be marked with `#[tree]` to use their
///   [`DisplayTree`] implementation instead.
///
/// - `#[ignore_field]` marks a field that should not be included in the tree.
///   When the tree is formatted, the field will not be present.
///
/// - `#[node_label]` causes a field to be used as the label of the node of the
///   tree that it is under. By default, the name of the `struct` or variant
///   will be used as the label. For example, for a variant representing a
///   binary operator and its arguments, you might want the operator to be the
///   used as the label of the tree.
///
/// - `#[field_label (= "label")]` causes the node for a field to have a label
///   in the form `label: value` when it is formatted. A string literal can be
///   passed to specify the label, otherwise the name of the field will be used.
///
/// ### Struct/Variant Attributes
///
/// - `#[node_label = "label"]` specifies the label to use for the node of the
///   tree. By default, the name of the `struct` or variant will be used.
///
/// # Examples
///
/// Implementing manually:
///
/// ```
/// use std::borrow::Cow;
///
/// use display_tree::{format_tree, DisplayTree, Style};
///
/// enum Tree {
///     A(i32, bool),
///     B(Box<Self>),
/// }
///
/// impl DisplayTree for Tree {
///     fn fmt_root(&self) -> Cow<str> {
///         match self {
///             Self::A(..) => Cow::Borrowed("A"),
///             Self::B(..) => Cow::Borrowed("B"),
///         }
///     }
///
///     fn fmt_leaves(&self, style: Style) -> Vec<String> {
///         match self {
///             Self::A(a, b) => vec![
///                 style.leaf_style.format(&a.to_string()),
///                 style.leaf_style.format(&b.to_string()),
///             ],
///             Self::B(tree) => vec![format_tree!(tree)],
///         }
///     }
/// }
/// ```
///
/// Specifying a field label:
///
/// ```
/// #[derive(display_tree::DisplayTree)]
/// struct Point {
///     #[field_label]
///     x: i32,
///     #[field_label]
///     y: i32,
/// }
/// ```
///
/// Ignoring a field:
///
/// ```
/// #[derive(display_tree::DisplayTree)]
/// struct Numbers {
///     not_so_secret_number: i32,
///     // `super_secret_number` not included when tree is formatted.
///     #[ignore_field]
///     super_secret_number: i32,
/// }
/// ```
///
/// Using a field as the node label:
///
/// ```
/// #[derive(display_tree::DisplayTree)]
/// enum Expr {
///     Num(i32),
///     BinOp {
///         // Show the operator as the node of this variant.
///         #[node_label]
///         op: char,
///         #[tree]
///         left: Box<Self>,
///         #[tree]
///         right: Box<Self>,
///     },
/// }
/// ```
///
/// Using a custom node label:
///
/// ```
/// #[derive(display_tree::DisplayTree)]
/// // Use "MyStruct" as the node label instead of the name of the `struct`.
/// #[node_label = "MyStruct"]
/// struct MyVeryLongComplexDetailedImportantStruct(bool);
/// ```
pub trait DisplayTree {
    /// Returns the string to be used as the root of this tree.
    ///
    /// [`fmt_root()`](DisplayTree::fmt_root) returs [`Cow<str>`] to avoid
    /// always allocating memory for an owned string when it is called. Often,
    /// the return value will just be a string literal, in which case
    /// [`fmt_root()`](DisplayTree::fmt_root) can return [`Cow::Borrowed`].
    ///
    /// [`fmt_root()`](DisplayTree::fmt_root) should generally not be called
    /// directly. It is used in default implementations of
    /// [`DisplayTree::fmt()`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// use display_tree::DisplayTree;
    ///
    /// struct Tree;
    ///
    /// impl DisplayTree for Tree {
    ///     fn fmt_root(&self) -> Cow<str> {
    ///         Cow::Borrowed("Tree")
    ///     }
    ///
    ///     // ...
    /// #   fn fmt_leaves(&self, style: display_tree::Style) -> Vec<String> {
    /// #       unimplemented!()
    /// #   }
    /// }
    /// ```
    fn fmt_root(&self) -> Cow<str>;

    /// Returns the leaves of this tree as strings.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{DisplayTree, Style};
    ///
    /// struct Point {
    ///     x: i32,
    ///     y: i32,
    /// }
    ///
    /// impl DisplayTree for Point {
    ///     // ...
    /// #   fn fmt_root(&self) -> std::borrow::Cow<str> {
    /// #       unimplemented!()
    /// #   }
    ///
    ///     fn fmt_leaves(&self, style: Style) -> Vec<String> {
    ///         vec![
    ///             style.leaf_style.format(&self.x.to_string()),
    ///             style.leaf_style.format(&self.y.to_string()),
    ///         ]
    ///     }
    /// }
    /// ```
    fn fmt_leaves(&self, style: Style) -> Vec<String>;

    /// Formats the tree using the given formatter and the given style.
    ///
    /// [`fmt()`](DisplayTree::fmt()) should generally not be called directly.
    /// It is used by [`AsTree`] and formatting macros to format a tree.
    ///
    /// [`fmt()`](DisplayTree::fmt()) has a default implementation, so you
    /// generally shouldn't have to implement this method, unless you want fine
    /// control over how the tree is formatted.
    fn fmt(&self, f: &mut fmt::Formatter, style: Style) -> fmt::Result {
        write!(f, "{}", style.leaf_style.format(&self.fmt_root()))?;

        let leaves = self.fmt_leaves(style);
        for (index, leaf) in leaves.iter().enumerate() {
            let s = style.leaf_style.format(leaf);
            let mut lines = s.lines();

            let branch = format!(
                "{}{} ",
                if index < leaves.len() - 1 {
                    style.char_set.connector
                } else {
                    style.char_set.end_connector
                },
                std::iter::repeat(style.char_set.horizontal)
                    .take(style.indentation as usize)
                    .collect::<String>()
            );

            write!(f, "\n{branch}{}", lines.next().unwrap_or_default())?;

            for line in lines {
                let branch = format!(
                    "{}{} ",
                    if index < leaves.len() - 1 {
                        style.char_set.vertical
                    } else {
                        ' '
                    },
                    " ".repeat(style.indentation as usize)
                );

                write!(f, "\n{branch}{}", line)?;
            }
        }

        Ok(())
        // let s = style.leaf_style.format(&::std::format!("{}", self.a));
        // let mut lines = s.lines();
        // write!(
        //     __display_tree_f,
        //     "\n{}{}",
        //     __display_tree_style.branch_style.format(&::std::format!(
        //         "{}{} ",
        //         __display_tree_style.char_set.connector,
        //         std::iter::repeat(__display_tree_style.char_set.horizontal)
        //             .take(__display_tree_style.indentation as usize)
        //             .collect::<String>()
        //     )),
        //     lines.next().unwrap_or_default(),
        // )?;
        // for line in lines {
        //     write!(
        //         __display_tree_f,
        //         "\n{}{}",
        //         __display_tree_style.branch_style.format(&::std::format!(
        //             "{}{} ",
        //             __display_tree_style.char_set.vertical,
        //             std::iter::repeat(' ')
        //                 .take(__display_tree_style.indentation as usize)
        //                 .collect::<String>()
        //         )),
        //         line,
        //     )?;
        // }
        // let s = ::std::format!(
        //     "{}",
        //     ::display_tree::AsTree::with_style(
        //         ::display_tree::to_display_tree_ref::ToDisplayTreeRef::to_display_tree(&self.b),
        //         __display_tree_style
        //     )
        // );
        // let mut lines = s.lines();
        // write!(
        //     __display_tree_f,
        //     "\n{}{}",
        //     __display_tree_style.branch_style.format(&::std::format!(
        //         "{}{} ",
        //         __display_tree_style.char_set.end_connector,
        //         std::iter::repeat(__display_tree_style.char_set.horizontal)
        //             .take(__display_tree_style.indentation as usize)
        //             .collect::<String>()
        //     )),
        //     lines.next().unwrap_or_default(),
        // )?;
        // for line in lines {
        //     write!(
        //         __display_tree_f,
        //         "\n{}{}",
        //         __display_tree_style.branch_style.format(&::std::format!(
        //             " {} ",
        //             std::iter::repeat(' ')
        //                 .take(__display_tree_style.indentation as usize)
        //                 .collect::<String>()
        //         )),
        //         line,
        //     )?;
        // }
        // Ok(())
    }
}

// impl<'a, T: DisplayTree> DisplayTree for &'a T {
//     fn fmt_root(&self) -> Cow<str> {
//         <T as DisplayTree>::fmt_root(self)
//     }

//     fn fmt_leaves(&self, style: Style) -> Vec<String> {
//         <T as DisplayTree>::fmt_leaves(self, style)
//     }
// }

// impl<T: DisplayTree> DisplayTree for Box<T> {
//     fn fmt_root(&self) -> Cow<str> {
//         <T as DisplayTree>::fmt_root(self)
//     }

//     fn fmt_leaves(&self, style: Style) -> Vec<String> {
//         <T as DisplayTree>::fmt_leaves(self, style)
//     }
// }

// impl<T: DisplayTree> DisplayTree for std::rc::Rc<T> {
//     fn fmt_root(&self) -> Cow<str> {
//         <T as DisplayTree>::fmt_root(self)
//     }

//     fn fmt_leaves(&self, style: Style) -> Vec<String> {
//         <T as DisplayTree>::fmt_leaves(self, style)
//     }
// }

// impl<T: DisplayTree> DisplayTree for std::sync::Arc<T> {
//     fn fmt_root(&self) -> Cow<str> {
//         <T as DisplayTree>::fmt_root(self)
//     }

//     fn fmt_leaves(&self, style: Style) -> Vec<String> {
//         <T as DisplayTree>::fmt_leaves(self, style)
//     }
// }
