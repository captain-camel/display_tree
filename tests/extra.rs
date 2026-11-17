use display_tree::*;

// Check generics
#[test]
fn struct_generic_param() {
    #[derive(DisplayTree)]
    struct Tree<Any> {
        a: Any,
        b: Any,
    }
    let tree: Tree<&str> = Tree { a: "A", b: "B" };

    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── A\n\
         └── B",
    );
    println!("{}", AsTree::new(&tree));
}

// Check generics
#[test]
fn struct_multiple_params() {
    #[derive(DisplayTree)]
    struct Tree {
        a: i32,
        b: i32,
        c: i32,
    }
    let tree: Tree = Tree { a: 1, b: 2, c: 3 };

    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        "Tree\n\
         ├── 1\n\
         ├── 2\n\
         └── 3",
    );
    println!("{}", AsTree::new(&tree));
}

//Typical rec call

#[test]
fn struct_recursive() {
    #[derive(DisplayTree)]
    struct Tree<T> {
        value: T,

        #[tree]
        #[optional_field]
        left: Option<Box<Tree<T>>>,

        #[tree]
        #[optional_field]
        right: Option<Box<Tree<T>>>,
    }
    let tree: Tree<i32> = Tree {
        value: 16,
        left: Some(Box::new(Tree {
            value: 32,
            left: Some(Box::new(Tree {
                value: 64,
                left: None,
                right: Some(Box::new(Tree {
                    value: 0,
                    left: None,
                    right: None,
                })),
            })),
            right: Some(Box::new(Tree {
                value: 128,
                left: None,
                right: Some(Box::new(Tree {
                    value: 256,
                    left: None,
                    right: None,
                })),
            })),
        })),
        right: None,
    };

    println!("{}", AsTree::new(&tree));
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "Tree\n",
            "├── 16\n",
            "└── Tree\n",
            "    ├── 32\n",
            "    ├── Tree\n",
            "    │   ├── 64\n",
            "    │   └── Tree\n",
            "    │       └── 0\n",
            "    └── Tree\n",
            "        ├── 128\n",
            "        └── Tree\n",
            "            └── 256"
        ),
    );
}

#[test]
fn struct_multiple() {
    #[derive(DisplayTree)]
    struct SubTree {
        o: i32,
        i: i32,
    }

    #[derive(DisplayTree)]
    struct Tree {
        #[tree]
        a: SubTree,
        b: i32,
        #[tree]
        c: SubTree,
    }
    let sub1: SubTree = SubTree { o: 069, i: 133 };
    let sub2: SubTree = SubTree { o: 300, i: 123 };

    let tree: Tree = Tree {
        a: sub1,
        b: 2,
        c: sub2,
    };

    println!("{}", AsTree::new(&tree));
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        //Must concat since it breaks with basic formatting (stripped trailing whitespace)
        concat!(
            "Tree\n",
            "├── SubTree\n",
            "│   ├── 69\n",
            "│   └── 133\n",
            "├── 2\n",
            "└── SubTree\n",
            "    ├── 300\n",
            "    └── 123",
        ),
    );
}

#[test]
fn struct_multiple_option() {
    #[derive(DisplayTree)]
    struct SubTree {
        #[node_label]
        o: i32,
        #[tree]
        #[optional_field]
        i: Option<Box<SubTree>>,
    }

    #[derive(DisplayTree)]
    struct Tree {
        #[tree]
        a: SubTree,
        #[node_label]
        b: i32,
        #[tree]
        c: SubTree,
    }
    let sub1: SubTree = SubTree {
        o: 069,
        i: Some(Box::new(SubTree {
            o: 42,
            i: Some(Box::new(SubTree { o: 1337, i: None })),
        })),
    };
    let sub2: SubTree = SubTree {
        o: 300,
        i: Some(Box::new(SubTree { o: 20, i: None })),
    };

    let tree: Tree = Tree {
        a: sub1,
        b: 8,
        c: sub2,
    };

    println!("{}", AsTree::new(&tree));
    assert_eq!(
        format!("{}", AsTree::new(&tree)),
        concat!(
            "8\n",
            "├── 69\n",
            "│   └── 42\n",
            "│       └── 1337\n",
            "└── 300\n",
            "    └── 20",
        ),
    );
}
