use std::fmt;

use super::DisplayTree;

/// A helper struct for formatting a type that implements [`DisplayTree`].
///
/// [`AsTree`] stores a reference to the type to format. It implements
/// [`std::fmt::Display`], so it can be used in [`println!`], [`format!`],
/// etc...
///
/// # Styling
///
/// [`AsTree`] controls the way a tree is styled when it is formatted. The style
/// can be customized using builder methods. See [`Style`] for the different
/// aspects that can be customized.
///
/// **Note:** [`StyleBuilder`] must be in scope to use the builder methods.
///
/// **Note:** Some styling options use ANSI escape codes and therefore will only
/// work where they are supported. See [`TextStyle`] for more information.
///
/// # Examples
///
/// ```
/// use display_tree::{AsTree, DisplayTree};
///
/// #[derive(DisplayTree)]
/// struct Tree {
///     a: i32,
///     b: i32,
/// }
///
/// let tree = Tree { a: 1, b: 2 };
///
/// assert_eq!(
///     format!("{}", AsTree::new(&tree)),
///     "Tree\n\
///      ├── 1\n\
///      └── 2"
/// );
/// ```
///
/// Specifying a style:
///
/// ```
/// use display_tree::{AsTree, CharSet, DisplayTree, StyleBuilder};
///
/// #[derive(DisplayTree)]
/// struct Tree {
///     a: i32,
///     b: i32,
/// }
///
/// let tree = Tree { a: 1, b: 2 };
///
/// assert_eq!(
///     format!("{}", AsTree::new(&tree).char_set(CharSet::DOUBLE_LINE)),
///     "Tree\n\
///      ╠══ 1\n\
///      ╚══ 2"
/// );
/// ```
pub struct AsTree<'a, T: DisplayTree> {
    tree: &'a T,
    style: Style,
}

impl<'a, T: DisplayTree> AsTree<'a, T> {
    /// Creates a wrapper around a type that implements [`DisplayTree`],
    /// allowing it to be formatted.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{AsTree, DisplayTree};
    ///
    /// #[derive(DisplayTree)]
    /// struct Tree;
    ///
    /// let as_tree = AsTree::new(&Tree);
    /// ```
    pub fn new(tree: &'a T) -> Self {
        Self {
            tree,
            style: Style::default(),
        }
    }

    /// Creates a wrapper around a type that implements [`DisplayTree`],
    /// allowing it to be formatted with the given style.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{AsTree, DisplayTree, Style};
    ///
    /// #[derive(DisplayTree)]
    /// struct Tree;
    ///
    /// let as_styled_tree = AsTree::with_style(&Tree, Style::default());
    /// ```
    pub fn with_style(tree: &'a T, style: Style) -> Self {
        Self { tree, style }
    }
}

impl<'a, T: DisplayTree> StyleBuilder for AsTree<'a, T> {
    fn style_mut(&mut self) -> &mut Style {
        &mut self.style
    }
}

impl<'a, T: DisplayTree> fmt::Display for AsTree<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.tree.fmt(f, self.style)
    }
}

/// A type that describes the way a type that implements [`DisplayTree`] should
/// be formatted.
///
/// Prefer using builder methods, either on [`Style`] or [`AsTree`], over
/// constructing an instance of [`Style`] manually.
///
/// **Note:** [`StyleBuilder`] must be in scope to use builder methods.
///
/// # Examples
///
/// ```
/// use display_tree::{CharSet, Color, Style, StyleBuilder};
///
/// let style = Style::default()
///     .leaf_color(Color::Blue)
///     .branch_background_color(Color::Red)
///     .indentation(4)
///     .char_set(CharSet::SINGLE_LINE_CURVED);
/// ```
#[derive(Clone, Copy)]
pub struct Style {
    /// The [`CharSet`] making up the branches of the tree.
    pub char_set: CharSet,
    /// The indentation of each node.
    pub indentation: u32,
    /// The style of the leaves of the tree. See [`TextStyle`] for more
    /// information.
    pub leaf_style: TextStyle,
    /// The style of the branches of the tree. See [`TextStyle`] for more
    /// information.
    pub branch_style: TextStyle,
}

impl StyleBuilder for Style {
    fn style_mut(&mut self) -> &mut Style {
        self
    }
}

impl Default for Style {
    /// The default [`Style`] used if none is specified.
    ///
    /// Default values:
    /// - [`char_set`](Style::char_set): [`CharSet::SINGLE_LINE`]
    /// - [`indentation`](Style::indentation): `2`
    /// - [`leaf_style`](Style::leaf_style): [`TextStyle::default()`]
    /// - [`branch_style`](Style::branch_style): [`TextStyle::default()`]
    fn default() -> Self {
        Self {
            char_set: CharSet::SINGLE_LINE,
            indentation: 2,
            leaf_style: TextStyle::default(),
            branch_style: TextStyle::default(),
        }
    }
}

/// A type that described how text will be rendered.
///
/// **Note:** [`TextStyle`] uses ANSI escape codes, so it should not be used
/// anywhere they are not supported. Support for individual fields may also vary
/// by terminal.
#[derive(Clone, Copy, Default)]
pub struct TextStyle {
    /// The text color. [`None`] will not apply any color
    pub text_color: Option<Color>,
    /// The background color. [`None`] will not apply any background color.
    pub background_color: Option<Color>,
    /// Whether the text is bold. (Might be rendered as increased intensity.)
    pub is_bold: bool,
    /// Whether the text has decreased intensity.
    pub is_faint: bool,
    /// Whether the text is italicised.
    pub is_italic: bool,
    /// Whether the text is underlined.
    pub is_underlined: bool,
    /// Whether the text is crossed-out.
    pub is_strikethrough: bool,
}

impl TextStyle {
    /// Applies the style to a string.
    ///
    /// [`apply()`](TextStyle::apply()) should not be called unless you are
    /// manually implementing [`DisplayTree`]. It is used in derived
    /// [`DisplayTree`] implementations.
    pub fn apply(&self, string: &str) -> String {
        use std::borrow::Cow;

        let mut ansi_codes: Vec<Cow<str>> = Vec::new();

        if let Some(text_color) = self.text_color {
            ansi_codes.push(match text_color {
                Color::Black => "30".into(),
                Color::Red => "31".into(),
                Color::Green => "32".into(),
                Color::Yellow => "33".into(),
                Color::Blue => "34".into(),
                Color::Magenta => "35".into(),
                Color::Cyan => "36".into(),
                Color::White => "37".into(),
                Color::Rgb(r, g, b) => format!("38;2;{r};{g};{b}").into(),
            })
        }

        if let Some(background_color) = self.background_color {
            ansi_codes.push(match background_color {
                Color::Black => "40".into(),
                Color::Red => "41".into(),
                Color::Green => "42".into(),
                Color::Yellow => "43".into(),
                Color::Blue => "44".into(),
                Color::Magenta => "45".into(),
                Color::Cyan => "46".into(),
                Color::White => "47".into(),
                Color::Rgb(r, g, b) => format!("48;2;{r};{g};{b}").into(),
            })
        }

        if self.is_bold {
            ansi_codes.push("1".into())
        }

        if self.is_faint {
            ansi_codes.push("2".into())
        }

        if self.is_italic {
            ansi_codes.push("3".into())
        }

        if self.is_underlined {
            ansi_codes.push("4".into())
        }

        if self.is_strikethrough {
            ansi_codes.push("9".into())
        }

        if !ansi_codes.is_empty() {
            let escape_sequences = ansi_codes
                .into_iter()
                .map(|code| format!("\x1b[{code}m"))
                .collect::<String>();
            format!("{escape_sequences}{string}\x1b[0m")
        } else {
            string.to_owned()
        }
    }
}

/// An ANSI color that a tree can be styled with.
#[derive(Clone, Copy)]
pub enum Color {
    /// ANSI color #0. Exact color depends on terminal.
    Black,
    /// ANSI color #1. Exact color depends on terminal.
    Red,
    /// ANSI color #2. Exact color depends on terminal.
    Green,
    /// ANSI color #3. Exact color depends on terminal.
    Yellow,
    /// ANSI color #4. Exact color depends on terminal.
    Blue,
    /// ANSI color #5. Exact color depends on terminal.
    Magenta,
    /// ANSI color #6. Exact color depends on terminal.
    Cyan,
    /// ANSI color #7. Exact color depends on terminal.
    White,
    /// A color with custom RGB values.
    ///
    /// **Note:** Truecolor support is required for this variant. [`Color::Rgb`]
    /// will not work properly if Truecolor is not supported in your terminal.
    /// In some cases it may be rendered as an 8-bit color if your terminal
    /// supports 256 colors.
    Rgb(u8, u8, u8),
}

/// A set of [`char`]s used for formatting a type that implements
/// [`DisplayTree`].
///
/// These are the characters that make up the text that connects the nodes of
/// the tree.
///
/// [`CharSet`] provides a few built-in sets via associated constants, but you
/// can construct your own if needed.
///
/// # Examples
///
/// ```
/// let char_set = display_tree::CharSet {
///     horizontal: '─',
///     vertical: '│',
///     connector: '├',
///     end_connector: '└',
/// };
/// ```
#[derive(Clone, Copy)]
pub struct CharSet {
    /// The characters used in the horizontal portion of a branch.
    ///
    /// Should resemble a plain horizontal line, eg. '─'.
    pub horizontal: char,
    /// The character used in the space between branches in place of
    /// [`connector`](CharSet::connector).
    ///
    /// Should resemble a plain vertical line, eg. '│'.
    pub vertical: char,
    /// The character connecting the vertical and horizontal portions of a
    /// branch.
    ///
    /// Should resemble a vertical line with an offshoot on the right, eg. '├'.
    pub connector: char,
    /// The character connecting the vertical and horizontal portions of the
    /// last branch under a node.
    ///
    /// Should resemble an "L" shape, eg. '└'.
    pub end_connector: char,
}

impl CharSet {
    /// Regular Unicode box-drawing characters.
    pub const SINGLE_LINE: Self = Self {
        horizontal: '─',
        vertical: '│',
        connector: '├',
        end_connector: '└',
    };

    /// Bold Unicode box-drawing characters.
    pub const SINGLE_LINE_BOLD: Self = Self {
        horizontal: '━',
        vertical: '┃',
        connector: '┣',
        end_connector: '┗',
    };

    /// Curved Unicode box-drawing characters.
    pub const SINGLE_LINE_CURVED: Self = Self {
        horizontal: '─',
        vertical: '│',
        connector: '├',
        end_connector: '╰',
    };

    /// Double Unicode box-drawing characters.
    pub const DOUBLE_LINE: Self = Self {
        horizontal: '═',
        vertical: '║',
        connector: '╠',
        end_connector: '╚',
    };

    /// ASCII characters.
    pub const ASCII: Self = Self {
        horizontal: '-',
        vertical: '|',
        connector: '|',
        end_connector: '`',
    };
}

/// A trait that provides builder methods for constructing an instance of
/// [`Style`].
///
/// [`StyleBuilder`] is implemented for [`Style`] and [`AsTree`], so you can use
/// those types to construct an instance of [`Style`].
///
/// Do not implement [`StyleBuilder`] for any new types.
pub trait StyleBuilder: Sized {
    #[doc(hidden)]
    fn style_mut(&mut self) -> &mut Style;

    /// Sets the [`CharSet`] making up the branches of the tree.
    ///
    /// See [`CharSet`] for more information.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{AsTree, CharSet, DisplayTree, StyleBuilder};
    ///
    /// #[derive(DisplayTree)]
    /// struct Tree {
    ///     a: i32,
    ///     b: i32,
    /// }
    ///
    /// let tree = Tree { a: 1, b: 2 };
    ///
    /// assert_eq!(
    ///     format!(
    ///         "{}",
    ///         // Use ASCII characters instead of the default Unicode ones.
    ///         AsTree::new(&tree).char_set(CharSet::ASCII),
    ///     ),
    ///     "Tree\n\
    ///      |-- 1\n\
    ///      `-- 2",
    /// );
    /// ```
    fn char_set(mut self, char_set: CharSet) -> Self {
        self.style_mut().char_set = char_set;
        self
    }

    /// Sets the indentation of each node.
    ///
    /// More specifically, [`indentation()`](AsTree::indentation()) sets the
    /// number of horizontal characters to use for each branch of the tree.
    ///
    /// # Examples
    ///
    /// ```
    /// use display_tree::{AsTree, DisplayTree, StyleBuilder};
    ///
    /// #[derive(DisplayTree)]
    /// struct Tree {
    ///     a: i32,
    ///     b: i32,
    /// }
    ///
    /// let tree = Tree { a: 1, b: 2 };
    ///
    /// assert_eq!(
    ///     format!("{}", AsTree::new(&tree).indentation(4),),
    ///     "Tree\n\
    ///      ├──── 1\n\
    ///      └──── 2"
    /// );
    /// ```
    fn indentation(mut self, indentation: u32) -> Self {
        self.style_mut().indentation = indentation;
        self
    }

    /// Sets the color of the leaves of the tree. See [`Color`] for more
    /// information.
    fn leaf_color(mut self, color: Color) -> Self {
        self.style_mut().leaf_style.text_color = Some(color);
        self
    }

    /// Sets the background color of the leaves of the tree. See [`Color`] for
    /// more information.
    fn leaf_background_color(mut self, color: Color) -> Self {
        self.style_mut().leaf_style.background_color = Some(color);
        self
    }

    /// Renders the leaves as bold.
    fn bold_leaves(mut self) -> Self {
        self.style_mut().leaf_style.is_bold = true;
        self
    }

    /// Decreases the intensity of the leaves.
    fn faint_leaves(mut self) -> Self {
        self.style_mut().leaf_style.is_faint = true;
        self
    }

    /// Italicises the leaves.
    fn italic_leaves(mut self) -> Self {
        self.style_mut().leaf_style.is_italic = true;
        self
    }

    /// Underlines the leaves.
    fn underlined_leaves(mut self) -> Self {
        self.style_mut().leaf_style.is_underlined = true;
        self
    }

    /// Causes the leaves to be crossed-out.
    fn strikethrough_leaves(mut self) -> Self {
        self.style_mut().leaf_style.is_strikethrough = true;
        self
    }

    /// Sets the color of the branches of the tree. See [`Color`] for more
    /// information.
    fn branch_color(mut self, color: Color) -> Self {
        self.style_mut().branch_style.text_color = Some(color);
        self
    }

    /// Sets the background color of the branches of the tree. See [`Color`] for
    /// more information.
    fn branch_background_color(mut self, color: Color) -> Self {
        self.style_mut().branch_style.background_color = Some(color);
        self
    }

    /// Renders the branches as bold.
    fn bold_branches(mut self) -> Self {
        self.style_mut().branch_style.is_bold = true;
        self
    }

    /// Decreases the intensity of the branches.
    fn faint_branches(mut self) -> Self {
        self.style_mut().branch_style.is_faint = true;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain() {
        let style = TextStyle::default();
        assert_eq!(style.apply("text"), "text")
    }

    #[test]
    fn text_color() {
        let style = TextStyle {
            text_color: Some(Color::Red),
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[31mtext\x1b[0m")
    }

    #[test]
    fn background_color() {
        let style = TextStyle {
            background_color: Some(Color::Red),
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[41mtext\x1b[0m")
    }

    #[test]
    fn bold() {
        let style = TextStyle {
            is_bold: true,
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[1mtext\x1b[0m")
    }

    #[test]
    fn faint() {
        let style = TextStyle {
            is_faint: true,
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[2mtext\x1b[0m")
    }

    #[test]
    fn italic() {
        let style = TextStyle {
            is_italic: true,
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[3mtext\x1b[0m")
    }

    #[test]
    fn underline() {
        let style = TextStyle {
            is_underlined: true,
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[4mtext\x1b[0m")
    }

    #[test]
    fn strikethrough() {
        let style = TextStyle {
            is_strikethrough: true,
            ..TextStyle::default()
        };
        assert_eq!(style.apply("text"), "\x1b[9mtext\x1b[0m")
    }
}
