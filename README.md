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
```

License: MIT OR Apache-2.0