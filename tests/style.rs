use display_tree::*;

#[test]
fn char_set() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree).char_set(CharSet::ASCII)),
        "Tree\n\
         |-- 1\n\
         `-- true",
    );
}

#[test]
fn indentation() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree).indentation(4)),
        "Tree\n\
         ├──── 1\n\
         └──── true",
    );
}
