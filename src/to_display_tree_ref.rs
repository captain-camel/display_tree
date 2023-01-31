//! A module containing the [`ToDisplayTreeRef`] trait and implementations
//! for //! [`std`] types.

use std::ops::Deref;

use super::DisplayTree;

/// A type that can be converted into a reference to a type that implements
/// [`DisplayTree`].
///
/// [`ToDisplayTreeRef`] is used to format fields in with a derived
/// [`DisplayTree`] implementation annotated with `#[tree]`. For example, it is
/// implemented for [`Box<impl DisplayTree>`] so that a boxed field can be
/// formatted as a tree.
///
/// [`ToDisplayTreeRef`] should generally not implemented for any new types,
/// unless you run into an edge case that is not covered by the implementations
/// provided by [`display_tree`].
pub trait ToDisplayTreeRef {
    type Tree: DisplayTree;
    /// Converts this type into a type which implements [`DisplayTree`].
    ///
    /// [`to_display_tree()`](ToDisplayTreeRef::to_display_tree()) should not
    /// be called directly. It is used by [`display_tree_derive`] in the
    /// code emitted by `derive(DisplayTree)`.
    fn to_display_tree(&self) -> &Self::Tree;
}

impl<T: DisplayTree, D: Deref<Target = T>> ToDisplayTreeRef for D {
    type Tree = T;

    fn to_display_tree(&self) -> &Self::Tree {
        Deref::deref(self)
    }
}

// TODO: just get reference in geneate code instead: Self::A { ref a}

// impl<T: DisplayTree> ToDisplayTreeRef for T {
//     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         self
//     }
// }

// impl<T: DisplayTree> ToDisplayTreeRef for &T {
//     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         self
//     }
// }

// impl<T: DisplayTree> ToDisplayTreeRef for Box<T> {
//     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         Deref::deref(self)
//     }
// }

// impl<T: DisplayTree> ToDisplayTreeRef for std::rc::Rc<T> {
//     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         Deref::deref(self)
//     }
// }

// impl<T: DisplayTree> ToDisplayTreeRef for std::sync::Arc<T> {
//     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         Deref::deref(self)
//     }
// }

// impl<'a, T: DisplayTree + Clone> ToDisplayTreeRef for std::borrow::Cow<'a, T>
// {     type Tree = T;

//     fn to_display_tree(&self) -> &T {
//         Deref::deref(self)
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_value() {
        #[derive(DisplayTree, PartialEq, Debug)]
        struct Tree;

        assert_eq!(Tree.to_display_tree(), &Tree)
    }

    #[test]
    fn from_reference() {
        #[derive(DisplayTree, PartialEq, Debug)]
        struct Tree;

        let reference = &Tree;
        assert_eq!(reference.to_display_tree(), &Tree)
    }

    #[test]
    fn from_box() {
        #[derive(DisplayTree, PartialEq, Debug)]
        struct Tree;

        assert_eq!(Box::new(Tree).to_display_tree(), &Tree)
    }

    #[test]
    fn from_rc() {
        #[derive(DisplayTree, PartialEq, Debug)]
        struct Tree;

        assert_eq!(std::rc::Rc::new(Tree).to_display_tree(), &Tree)
    }

    #[test]
    fn from_arc() {
        #[derive(DisplayTree, PartialEq, Debug)]
        struct Tree;

        assert_eq!(std::sync::Arc::new(Tree).to_display_tree(), &Tree)
    }

    #[test]
    fn from_cow() {
        #[derive(DisplayTree, PartialEq, Debug, Clone)]
        struct Tree;

        let cow: std::borrow::Cow<Tree> = std::borrow::Cow::Owned(Tree);
        assert_eq!(cow.to_display_tree(), &Tree)
    }
}
