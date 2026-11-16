# `display_tree`

Simple, automatic, and customizable tree pretty-printing in Rust.

![Example](https://i.ibb.co/RbpZ0Jk/Screenshot-2022-12-19-at-5-36-09-PM.png)

This crate provides tools to easily pretty-print data as a tree, including a trait that represents the ability to be printed as a tree, and a derive macro to automatically implement it for your types

See the [crate-level documentation](https://docs.rs/display_tree/*/display_tree) to get started.

## Examples

```rust
use display_tree::{AsTree, CharSet, DisplayTree, StyleBuilder};

// A tree representing a numerical expression.
#[derive(DisplayTree)]
enum Expr {
    Int(i32),
    BinOp {
        #[node_label]
        op: char,
        #[tree]
        left: Box<Self>,
        #[tree]
        right: Box<Self>,
    },
    UnaryOp {
        #[node_label]
        op: char,
        #[tree]
        arg: Box<Self>,
    },
}

let expr: Expr = Expr::BinOp {
    op: '+',
    left: Box::new(Expr::UnaryOp {
        op: '-',
        arg: Box::new(Expr::Int(2)),
    }),
    right: Box::new(Expr::Int(7)),
};

assert_eq!(
    format!(
        "{}",
        AsTree::new(&expr)
            .indentation(1)
            .char_set(CharSet::DOUBLE_LINE)
    ),
    concat!(
        "+\n",
        "╠═ -\n",
        "║  ╚═ Int\n",
        "║     ╚═ 2\n",
        "╚═ Int\n",
        "   ╚═ 7",
    ),
);


//A recursive tree with multiple branches and unknown generic parameter.
#[derive(DisplayTree)]
struct RecTree<T> {
        
        value: T,
        
        #[tree]
        #[optional_field] //Denotes Option (None as base case).
        left: Option<Box<RecTree<T>>>,
                
        #[tree]
        #[optional_field]
        right: Option<Box<RecTree<T>>>,

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

//A tree with seperate recursive subtrees
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
let sub_tree_1: SubTree = SubTree {
        o: 32,
        i: Some(Box::new(SubTree {
            o: 64,
            i: Some(Box::new(SubTree {
                o: 128,
                i: None,
            })),
        })),
        
        
};
let sub_tree_2: SubTree = SubTree {
        o: 300,
        i: Some(Box::new(SubTree {
            o: 200,
            i: None,
        })),
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
```