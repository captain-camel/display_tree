use display_tree::*;

#[test]
fn field_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── 1\n\
         └── true",
    );
}

#[test]
fn tuple_struct() {
    #[derive(DisplayTree)]
    struct Tree(i32, bool);

    let tree = Tree(1, true);
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── 1\n\
         └── true",
    );
}

#[test]
fn enum_unit() {
    #[derive(DisplayTree)]
    enum Tree {
        A,
    }

    let tree = Tree::A;
    assert_eq!(format!("{}", AsTree::new(&tree)), "A");
}

#[test]
fn enum_unnamed() {
    #[derive(DisplayTree)]
    enum Tree {
        A(i32, bool),
    }

    let tree = Tree::A(1, true);
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "A\n\
         ├── 1\n\
         └── true",
    );
}

#[test]
fn enum_named() {
    #[derive(DisplayTree)]
    enum Tree {
        A { a: i32, b: bool },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "A\n\
         ├── 1\n\
         └── true",
    );
}
