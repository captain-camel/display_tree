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
pub mod to_display_tree_ref;

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
///
/// The [`AsTree`] type should be used to display a tree implementing
/// [`DisplayTree`]. See the [`AsTree` documentation](AsTree) for more
/// information.
///
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
/// [`derive(DisplayTree)`] provies a few helper attribute that allow the
/// derived implementation to be customized.
///
/// ### Field Attributes
///
/// - `#[tree]` marks a field that should be formatted as a tree. By default, a
///   field's [`std::fmt::Display`] implementation will be used to format it in
///   the tree, but fields can be marked with `#[tree]` to use their
///   [`DisplayTree`] implementation instead. `#[tree]` can be used on any type
///   that conforms to
///   [`ToDisplayTreeRef`](to_display_tree_ref::ToDisplayTreeRef), which
///   includes any types that conform to [`DisplayTree`].
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
    /// Formats the tree using the given formatter and the given style.
    ///
    /// [`fmt()`](DisplayTree::fmt()) should not be called directly. It is used
    /// by [`AsTree`] to format a tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{AsTree, DisplayTree, Style};
    ///
    /// struct Point {
    ///     x: i32,
    ///     y: i32,
    /// }
    ///
    /// impl DisplayTree for Point {
    ///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, style: Style) -> std::fmt::Result {
    ///         writeln!(f, "Point")?;
    ///         writeln!(
    ///             f,
    ///             "{}{} x: {}",
    ///             style.char_set.connector,
    ///             std::iter::repeat(style.char_set.horizontal)
    ///                 .take(style.indentation as usize)
    ///                 .collect::<String>(),
    ///             self.x
    ///         )?;
    ///         write!(
    ///             f,
    ///             "{}{} y: {}",
    ///             style.char_set.end_connector,
    ///             std::iter::repeat(style.char_set.horizontal)
    ///                 .take(style.indentation as usize)
    ///                 .collect::<String>(),
    ///             self.y
    ///         )
    ///     }
    /// }
    ///
    /// assert_eq!(
    ///     format!("{}", AsTree::new(&Point { x: 10, y: 20 })),
    ///     "Point\n\
    ///      ├── x: 10\n\
    ///      └── y: 20",
    /// );
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter, style: Style) -> fmt::Result;
}

/// Prints a type that implements [`DisplayTree`] to the standard output as a
/// tree.
///
/// A [`Style`] can be passed as the second argument to customize the way the
/// tree is formatted.
///
/// # Examples
///
/// ```
/// # #[derive(display_tree::DisplayTree)]
/// # struct Tree;
/// # let tree = Tree;
/// use display_tree::print_tree;
/// print_tree!(tree);
/// ```
///
/// Specifying a style:
///
/// ```
/// # #[derive(display_tree::DisplayTree)]
/// # struct Tree;
/// # let tree = Tree;
/// use display_tree::{print_tree, Style, StyleBuilder};
/// print_tree!(tree, Style::default().indentation(1));
/// ```
#[macro_export]
macro_rules! print_tree {
    ($tree:expr $(,)?) => {
        ::std::print!("{}", $crate::AsTree::new(&$tree))
    };
    ($tree:expr, $style:expr $(,)?) => {
        ::std::print!("{}", $crate::AsTree::with_style(&$tree, $style))
    };
}

/// Prints a type that implements [`DisplayTree`] to the standard output as a
/// tree, with a newline.
///
/// A [`Style`] can be passed as the second argument to customize the way the
/// tree is formatted.
///
/// # Examples
///
/// ```
/// # #[derive(display_tree::DisplayTree)]
/// # struct Tree;
/// # let tree = Tree;
/// use display_tree::println_tree;
/// println_tree!(tree)
/// ```
///
/// Specifying a style:
///
/// ```
/// # #[derive(display_tree::DisplayTree)]
/// # struct Tree;
/// # let tree = Tree;
/// use display_tree::{println_tree, Style, StyleBuilder};
/// println_tree!(tree, Style::default().indentation(1));
/// ```
#[macro_export]
macro_rules! println_tree {
    ($tree:expr $(,)?) => {
        ::std::println!("{}", $crate::AsTree::new(&$tree))
    };
    ($tree:expr, $style:expr $(,)?) => {
        ::std::println!("{}", $crate::AsTree::with_style(&$tree, $style))
    };
}

/// Writes a type that implements [`DisplayTree`] to a buffer as a tree.
///
/// A [`Style`] can be passed as the second argument to customize the way the
/// tree is formatted.
///
/// # Examples
///
/// ```
/// # use std::io::Write;
/// use display_tree::write_tree;
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree;
///
/// let mut buf = Vec::new();
/// write_tree!(&mut buf, Tree);
///
/// assert_eq!(&buf, "Tree".as_bytes());
/// ```
///
/// Specifying a style:
///
/// ```
/// # use std::io::Write;
/// use display_tree::{write_tree, CharSet, Style, StyleBuilder};
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree {
///     a: i32,
///     b: bool,
/// }
///
/// let mut buf = Vec::new();
/// write_tree!(
///     &mut buf,
///     Tree { a: 1, b: true },
///     Style::default().char_set(CharSet::SINGLE_LINE_CURVED)
/// );
///
/// assert_eq!(
///     &buf,
///     "Tree\n\
///      ├── 1\n\
///      ╰── true"
///         .as_bytes()
/// );
/// ```
#[macro_export]
macro_rules! write_tree {
    ($f:expr, $tree:expr $(,)?) => {
        ::std::write!($f, "{}", $crate::AsTree::new(&$tree))
    };
    ($f:expr, $tree:expr, $style:expr $(,)?) => {
        ::std::write!($f, "{}", $crate::AsTree::with_style(&$tree, $style))
    };
}

/// Writes a type that implements [`DisplayTree`] to a buffer as a tree, with a
/// newline.
///
/// A [`Style`] can be passed as the second argument to customize the way the
/// tree is formatted.
///
/// # Examples
///
/// ```
/// # use std::io::Write;
/// use display_tree::writeln_tree;
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree;
///
/// let mut buf = Vec::new();
/// writeln_tree!(&mut buf, Tree);
///
/// assert_eq!(&buf, "Tree\n".as_bytes());
/// ```
///
/// Specifying a style:
///
/// ```
/// # use std::io::Write;
/// use display_tree::{writeln_tree, CharSet, Style, StyleBuilder};
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree {
///     a: i32,
///     b: bool,
/// }
///
/// let mut buf = Vec::new();
/// writeln_tree!(
///     &mut buf,
///     Tree { a: 1, b: true },
///     Style::default().char_set(CharSet::SINGLE_LINE_BOLD)
/// );
///
/// assert_eq!(
///     &buf,
///     "Tree\n\
///      ┣━━ 1\n\
///      ┗━━ true\n"
///         .as_bytes()
/// );
/// ```
#[macro_export]
macro_rules! writeln_tree {
    ($f:expr, $tree:expr $(,)?) => {
        ::std::writeln!($f, "{}", $crate::AsTree::new(&$tree))
    };
    ($f:expr, $tree:expr, $style:expr $(,)?) => {
        ::std::writeln!($f, "{}", $crate::AsTree::with_style(&$tree, $style))
    };
}

/// Creates a [`String`] from a type that implements [`DisplayTree`], formatting
/// it as a tree.
///
/// A [`Style`] can be passed as the second argument to customize the way the
/// tree is formatted.
///
/// # Examples
///
/// ```
/// use display_tree::format_tree;
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree;
///
/// assert_eq!(format_tree!(Tree), "Tree")
/// ```
///
/// Specifying a style:
///
/// ```
/// use display_tree::{format_tree, Style, StyleBuilder};
///
/// #[derive(display_tree::DisplayTree)]
/// struct Tree {
///     a: i32,
///     b: bool,
/// }
///
/// assert_eq!(
///     format_tree!(Tree { a: 1, b: true }, Style::default().indentation(1)),
///     "Tree\n\
///      ├─ 1\n\
///      └─ true"
/// );
/// ```
#[macro_export]
macro_rules! format_tree {
    ($tree:expr $(,)?) => {
        ::std::format!("{}", $crate::AsTree::new(&$tree))
    };
    ($tree:expr, $style:expr $(,)?) => {
        ::std::format!("{}", $crate::AsTree::with_style(&$tree, $style))
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn write() {
        use std::io::Write;

        #[derive(display_tree::DisplayTree)]
        struct Tree;

        let mut buf = Vec::new();
        display_tree::write_tree!(&mut buf, Tree).unwrap();

        assert_eq!(&buf, "Tree".as_bytes());
    }

    #[test]
    fn writeln() {
        use std::io::Write;

        #[derive(display_tree::DisplayTree)]
        struct Tree;

        let mut buf = Vec::new();
        display_tree::writeln_tree!(&mut buf, Tree).unwrap();

        assert_eq!(&buf, "Tree\n".as_bytes());
    }

    #[test]
    fn format() {
        #[derive(display_tree::DisplayTree)]
        struct Tree;

        assert_eq!(display_tree::format_tree!(Tree), "Tree")
    }
}
