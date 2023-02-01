use display_tree::*;

#[test]
fn node_attr_node_label_struct() {
    #[derive(DisplayTree)]
    #[node_label = "label"]
    struct Tree {
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "label\n\
         ├── 1\n\
         └── true",
    );
}

#[test]
fn node_attr_node_label_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        #[node_label = "label"]
        A { a: i32, b: bool },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "label\n\
         ├── 1\n\
         └── true",
    );
}

#[test]
fn field_attr_field_label_no_args_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        #[field_label]
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── a: 1\n\
         └── true",
    );
}

#[test]
fn field_attr_field_label_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        #[field_label = "label"]
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── label: 1\n\
         └── true",
    );
}

#[test]
fn field_attr_field_label_no_args_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        A {
            #[field_label]
            a: i32,
            b: bool,
        },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "A\n\
         ├── a: 1\n\
         └── true",
    );
}

#[test]
fn field_attr_field_label_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        A {
            #[field_label = "label"]
            a: i32,
            b: bool,
        },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "A\n\
         ├── label: 1\n\
         └── true",
    );
}

#[allow(dead_code)]
#[test]
fn field_attr_ignore_field_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        #[ignore_field]
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         └── 1",
    );
}

#[test]
fn field_attr_ignore_field_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        A {
            a: i32,
            #[ignore_field]
            b: bool,
        },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "A\n\
         └── 1",
    );
}

#[test]
fn field_attr_node_label_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        #[node_label]
        a: i32,
        b: bool,
    }

    let tree = Tree { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "1\n\
         └── true",
    );
}

#[test]
fn field_attr_node_label_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        A {
            #[node_label]
            a: i32,
            b: bool,
        },
    }

    let tree = Tree::A { a: 1, b: true };
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "1\n\
         └── true",
    );
}

#[test]
fn field_attr_tree_struct() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        #[tree]
        b: Inner,
    }

    #[derive(DisplayTree)]
    struct Inner {
        a: i32,
        b: bool,
    }

    let tree = Tree {
        a: 1,
        b: Inner { a: 2, b: true },
    };
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "Tree\n",
            "├── 1\n",
            "└── Inner\n",
            "    ├── 2\n",
            "    └── true",
        ),
    );
}

#[test]
fn field_attr_tree_enum() {
    #[derive(DisplayTree)]
    enum Tree {
        A(#[tree] Inner),
    }

    #[derive(DisplayTree)]
    struct Inner {
        a: i32,
        b: bool,
    }

    let tree = Tree::A(Inner { a: 1, b: true });
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "A\n",
            "└── Inner\n",
            "    ├── 1\n",
            "    └── true"
        ),
    );
}

#[test]
fn field_attr_tree_ref() {
    #[derive(DisplayTree)]
    enum Tree<'a> {
        A { a: i32, b: bool },
        B(#[tree] &'a Self),
    }

    let tree = Tree::B(&Tree::A { a: 1, b: true });
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "B\n",
            "└── A\n",
            "    ├── 1\n",
            "    └── true",
        ),
    );
}

#[test]
fn field_attr_tree_box() {
    #[derive(DisplayTree)]
    enum Tree {
        A { a: i32, b: bool },
        B(#[tree] Box<Self>),
    }

    let tree = Tree::B(Box::new(Tree::A { a: 1, b: true }));
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "B\n",
            "└── A\n",
            "    ├── 1\n",
            "    └── true",
        ),
    );
}

#[test]
fn field_attr_tree_rc() {
    use std::rc::Rc;

    #[derive(DisplayTree)]
    enum Tree {
        A { a: i32, b: bool },
        B(#[tree] Rc<Self>),
    }

    let tree = Tree::B(Rc::new(Tree::A { a: 1, b: true }));
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "B\n",
            "└── A\n",
            "    ├── 1\n",
            "    └── true"
        )
    );
}

#[test]
fn field_attr_tree_arc() {
    use std::sync::Arc;

    #[derive(DisplayTree)]
    enum Tree {
        A { a: i32, b: bool },
        B(#[tree] Arc<Self>),
    }

    let tree = Tree::B(Arc::new(Tree::A { a: 1, b: true }));
    #[rustfmt::skip]
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "B\n",
            "└── A\n",
            "    ├── 1\n",
            "    └── true"
        )
    );
}
