#![allow(unused)]

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct NodeName(String);
#[derive(Debug, PartialEq)]
struct VariableName(String);

#[derive(Debug, PartialEq)]
struct Choice {
    text: String,
    target: NodeName,
    condition: Option<Conditional>,
}

#[derive(Debug, PartialEq)]
enum Step {
    Dialogue(String, Vec<Choice>),
    Command(String),
    Assign(VariableName, Expr),
    Conditional(Conditional, Vec<Step>, Vec<(Conditional, Vec<Step>)>, Vec<Step>),
    Jump(NodeName),
    Stop,
}

#[derive(Debug, PartialEq)]
enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Term(Term),
}

#[derive(Debug, PartialEq)]
enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, PartialEq)]
enum BinaryOp {
    And,
    Or,
    Plus,
    Minus,
    Multiply,
    Divide,
    Equals,
    NotEquals,
}

#[derive(Debug, PartialEq)]
enum Term {
    Number(f32),
    Boolean(bool),
    String(String),
    Variable(VariableName),
    Function(String, Vec<Expr>),
}

#[derive(Debug, PartialEq)]
enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
}

#[derive(Debug, PartialEq)]
enum Conditional {
    Truthy(Expr),
    Comparison(Expr, Comparison, Expr),
    Tmp(String),
}

#[derive(Debug, PartialEq)]
pub struct Node {
    title: NodeName,
    extra: HashMap<String, String>,
    steps: Vec<Step>,
    visited: bool,
}

pub struct YarnEngine {
    nodes: Vec<Node>,
}

impl YarnEngine {
}

fn parse_expr(tokenizer: &mut TokenIterator) -> Result<Expr, ()> {
    let t = tokenizer.next().ok_or(())?;
    let left = match t {
        Token::Number(num) => Expr::Term(Term::Number(num)),
        Token::ExclamationMark => {
            let expr = parse_expr(tokenizer)?;
            Expr::Unary(UnaryOp::Not, Box::new(expr))
        }
        Token::Minus => {
            let expr = parse_expr(tokenizer)?;
            Expr::Unary(UnaryOp::Negate, Box::new(expr))
        }
        Token::DollarSign => {
            let name = match tokenizer.next().ok_or(())? {
                Token::Word(name) => name,
                _ => return Err(()),
            };
            Expr::Term(Term::Variable(VariableName(name)))
        }
        _ => return Err(()),
    };
    if tokenizer.peek().is_none() {
        return Ok(left);
    }

    let op = match tokenizer.next().unwrap() {
        Token::Plus => BinaryOp::Plus,
        Token::Minus => BinaryOp::Minus,
        Token::Star => BinaryOp::Multiply,
        Token::Slash => BinaryOp::Divide,
        Token::ExclamationMark => {
            if tokenizer.next().ok_or(())? != Token::Equals {
                return Err(());
            }
            BinaryOp::NotEquals
        }
        Token::Equals => {
            if tokenizer.next().ok_or(())? != Token::Equals {
                return Err(());
            }
            BinaryOp::Equals
        }
        Token::Word(word) => {
            match &*word {
                "and" => BinaryOp::And,
                "or" => BinaryOp::Or,
                "eq" => BinaryOp::Equals,
                "neq" => BinaryOp::NotEquals,
                _ => return Err(()),
            }
        }
        _ => return Err(()),
    };
    let right = parse_expr(tokenizer)?;
    Ok(Expr::Binary(op, Box::new(left), Box::new(right)))
}

enum Line {
    Dialogue(String),
    If(String),
    ElseIf(String),
    Else,
    EndIf,
    Action(String),
    Option(Option<String>, NodeName),
    InlineOption(String),
}

fn parse_line(tokenizer: &mut TokenIterator) -> Result<(u32, Line), ()> {
    let t = tokenizer.next().ok_or(())?;
    let indent = tokenizer.last_indent();
    do_parse_line(t, tokenizer).map(|l| (indent, l))
}

fn do_parse_line(token: Token, tokenizer: &mut TokenIterator) -> Result<Line, ()> {
    match token {
        Token::Word(mut text) => {
            let rest = tokenizer.remainder_of_line().ok_or(())?;
            text += &rest;
            return Ok(Line::Dialogue(text));
        }
        Token::LeftAngle => {
            if tokenizer.next().ok_or(())? != Token::LeftAngle {
                return Err(());
            }
            let rest = parse_string_until(tokenizer, '>')?;
            if tokenizer.next().ok_or(())? != Token::RightAngle {
                return Err(());
            }
            if rest.starts_with("if ") {
                return Ok(Line::If(rest[3..].trim().to_owned()));
            }
            if rest.starts_with("elseif ") {
                return Ok(Line::ElseIf(rest[7..].trim().to_owned()));
            }
            if rest.trim() == "else" {
                return Ok(Line::Else);
            }
            if rest.trim() == "endif" {
                return Ok(Line::EndIf);
            }
            return Ok(Line::Action(rest.trim().to_owned()));
        }
        Token::LeftBracket => {
            if tokenizer.next().ok_or(())? != Token::LeftBracket {
                return Err(());
            }
            let contents = parse_string_until(tokenizer, ']')?;
            if tokenizer.next().ok_or(())? != Token::RightBracket {
                return Err(());
            }
            let mut parts = contents.split('|');
            let first = parts.next().unwrap();
            let second = parts.next();
            if let Some(second) = second {
                return Ok(Line::Option(Some(first.to_string()), NodeName(second.to_string())));
            }
            return Ok(Line::Option(None, NodeName(first.to_string())));
        }
        Token::Minus => {
            if tokenizer.next().ok_or(())? != Token::RightAngle {
                return Err(());
            }
            let rest = tokenizer.remainder_of_line().ok_or(())?;
            return Ok(Line::InlineOption(rest));
        }
        _ => return Err(()),
    }
}

fn parse_string_until(tokenizer: &mut TokenIterator, until: char) -> Result<String, ()> {
    let mut buffer = String::new();
    loop {
        let ch = tokenizer.next_char().ok_or(())?;
        if ch == until {
            return Ok(buffer);
        }
        buffer.push(ch);
    }
}

#[derive(Copy, Clone, PartialEq)]
enum StepPhase {
    Toplevel,
    Conditional,
}

#[derive(Debug)]
enum DialogueOption {
    Inline(String),
    External(String, NodeName),
}

fn try_parse_option(tokenizer: &mut TokenIterator) -> Result<Option<DialogueOption>, ()> {
    let t = match tokenizer.peek() {
        Some(t) => t,
        None => return Ok(None),
    };
    if t == '[' || t == '-' {
        let (_indent, line) = parse_line(tokenizer)?;
        match line {
            Line::Option(Some(text), name) => Ok(Some(DialogueOption::External(text, name))),
            Line::InlineOption(s) => Ok(Some(DialogueOption::Inline(s))),
            _ => unreachable!(),
        }
    } else {
        Ok(None)
    }
}

struct ConditionalParts {
    if_steps: Vec<Step>,
    else_ifs: Vec<(Conditional, Vec<Step>)>,
    else_steps: Vec<Step>,
}

#[derive(PartialEq)]
enum ConditionalParsePhase {
    If,
    ElseIf,
    Else,
}

fn parse_conditional(tokenizer: &mut TokenIterator) -> Result<ConditionalParts, ()> {
    let mut parts = ConditionalParts {
        if_steps: vec![],
        else_ifs: vec![],
        else_steps: vec![],
    };
    let mut phase = ConditionalParsePhase::If;
    loop {
        let (_index, line) = parse_line(tokenizer)?;
        match line {
            Line::ElseIf(s) => {
                if phase == ConditionalParsePhase::Else {
                    return Err(());
                }
                phase = ConditionalParsePhase::ElseIf;
                parts.else_ifs.push((Conditional::Tmp(s), vec![]));
            }
            Line::Else => {
                if phase == ConditionalParsePhase::Else {
                    return Err(());
                }
                phase = ConditionalParsePhase::Else;
            }
            Line::EndIf => {
                return Ok(parts);
            }
            l => {
                let step = parse_toplevel_line(tokenizer, l)?;
                let steps = match phase {
                    ConditionalParsePhase::If => &mut parts.if_steps,
                    ConditionalParsePhase::ElseIf => &mut parts.else_ifs.iter_mut().last().unwrap().1,
                    ConditionalParsePhase::Else => &mut parts.else_steps,
                };
                steps.push(step);
            }
        }
    }
}

fn parse_toplevel_line(tokenizer: &mut TokenIterator, line: Line) -> Result<Step, ()> {
    match line {
        Line::Dialogue(s) => {
            println!("found dialogue '{}'", s);
            let mut choices = vec![];
            loop {
                let opt = try_parse_option(tokenizer)?;
                println!("found opt {:?}", opt);
                match opt {
                    Some(DialogueOption::Inline(s)) => {
                        return Err(()); //TODO
                    }
                    Some(DialogueOption::External(text, node)) => {
                        choices.push(Choice {
                            text: text,
                            target: node,
                            condition: None, //TODO
                        });
                    }
                    None => break,
                }
            }
            return Ok(Step::Dialogue(s, choices));
        }
        Line::If(s) => {
            let parts = parse_conditional(tokenizer)?;
            return Ok(Step::Conditional(Conditional::Tmp(s),
                                        parts.if_steps,
                                        parts.else_ifs,
                                        parts.else_steps));
        }
        Line::Action(s) => {
            if s.starts_with("set ") {
                let rest = &s[4..].trim();
                let var_end = rest.find(' ').ok_or(())?;
                let var = &rest[0..var_end];
                let mut tokenizer = TokenIterator::new(&rest[var_end..]);
                let expr = parse_expr(&mut tokenizer)?;
                return Ok(Step::Assign(VariableName(var.to_string()), expr));
            }
            return Ok(Step::Command(s));
        }
        Line::EndIf |
        Line::ElseIf(_) |
        Line::Else |
        Line::EndIf |
        Line::Option(..) |
        Line::InlineOption(..) =>
            return Err(())
    }
}


fn parse_step(tokenizer: &mut TokenIterator) -> Result<Step, ()> {
    let (_indent, line) = parse_line(tokenizer)?;
    parse_toplevel_line(tokenizer, line)
}

fn parse_node_contents(tokenizer: &mut TokenIterator) -> Result<Vec<Step>, ()> {
    let mut steps = vec![];
    loop {
        match tokenizer.peek().ok_or(())? {
            '=' => {
                let _ = tokenizer.next();
                if tokenizer.next() != Some(Token::Equals) {
                    return Err(());
                }
                if tokenizer.next() != Some(Token::Equals) {
                    return Err(());
                }
                return Ok(steps);
            }
            _ => steps.push(parse_step(tokenizer)?),
        }
    }
}

fn parse_node(tokenizer: &mut TokenIterator) -> Result<Node, ()> {
    let mut node = Node {
        title: NodeName(String::new()),
        extra: HashMap::new(),
        steps: vec![],
        visited: false,
    };
    loop {
        let t = tokenizer.next().ok_or(())?;
        match t {
            Token::Word(name) => {
                let value = tokenizer.remainder_of_line().ok_or(())?;
                if name == "title:" {
                    if !node.title.0.is_empty() {
                        return Err(());
                    }
                    node.title.0 = value.trim().to_string();
                } else {
                    node.extra.insert(name[..name.len()-1].to_string(), value.trim().to_string());
                }
            }
            Token::Minus => {
                if tokenizer.next().ok_or(())? != Token::Minus {
                    return Err(());
                }
                if tokenizer.next().ok_or(())? != Token::Minus {
                    return Err(());
                }
                node.steps = parse_node_contents(tokenizer)?;
                return Ok(node);
            }
            _ => return Err(()),
        }
    }
}

fn parse_nodes(tokenizer: &mut TokenIterator) -> Result<Vec<Node>, ()> {
    let mut nodes = vec![];
    while tokenizer.peek().is_some() {
        nodes.push(parse_node(tokenizer)?);
    }
    Ok(nodes)
}

#[derive(Debug, PartialEq)]
enum Token {
    DollarSign,
    LeftAngle,
    RightAngle,
    Equals,
    Pipe,
    Plus,
    Minus,
    Star,
    Slash,
    ExclamationMark,
    LeftBracket,
    RightBracket,
    Number(f32),
    Word(String),
    StartNode,
}

struct TokenIterator<'a> {
    input: Box<Iterator<Item=char> + 'a>,
    last_char: Option<char>,
    last_indent: u32,
    start_of_line: bool,
}

impl<'a> TokenIterator<'a> {
    fn new(input: &'a str) -> TokenIterator<'a> {
        TokenIterator {
            input: Box::new(input.chars()),
            last_char: None,
            last_indent: 0,
            start_of_line: true,
        }
    }

    fn peek(&mut self) -> Option<char> {
        let mut ch;
        loop {
            ch = self.next_char();
            if ch != Some('\n') {
                break;
            }
        }
        self.last_char = ch;
        ch
    }

    fn remainder_of_line(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(ch) = self.next_char() {
            if ch == '\n' {
                return Some(buffer);
            }
            buffer.push(ch);
        }
        if buffer.is_empty() {
            return None;
        }
        return Some(buffer);
    }

    fn next_char(&mut self) -> Option<char> {
        self.last_char.take().or_else(|| self.input.next())
    }

    fn push_back(&mut self, ch: char) {
        assert!(self.last_char.is_none());
        self.last_char = Some(ch);
    }

    fn last_indent(&self) -> u32 {
        self.last_indent
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let mut buffer = String::new();
        loop {
            let mut ch = self.next_char()?;
            if ch != ' ' {
                self.start_of_line = false;
            }
            match ch {
                '|' => return Some(Token::Pipe),
                '$' => return Some(Token::DollarSign),
                '<' => return Some(Token::LeftAngle),
                '>' => return Some(Token::RightAngle),
                '=' => return Some(Token::Equals),
                '-' => return Some(Token::Minus),
                '!' => return Some(Token::ExclamationMark),
                '[' => return Some(Token::LeftBracket),
                ']' => return Some(Token::RightBracket),
                '0'...'9' => {
                    buffer.push(ch);
                    let mut before_decimal = true;
                    loop {
                        let ch = match self.next_char() {
                            Some(ch) => ch,
                            None => break,
                        };
                        match ch {
                            '0'...'9' => buffer.push(ch),
                            '.' if before_decimal => {
                                before_decimal = false;
                                buffer.push(ch);
                            }
                            ch => {
                                self.push_back(ch);
                                break;
                            }
                        }
                    }
                    return Some(Token::Number(buffer.parse().ok()?));
                }
                ' ' if self.start_of_line => self.last_indent += 1,
                ' ' => (),
                '\n' => {
                    self.start_of_line = true;
                    self.last_indent = 0;
                }
                ch => {
                    buffer.push(ch);
                    loop {
                        let ch = match self.next_char() {
                            Some(ch) => ch,
                            None if buffer.is_empty() => return None,
                            None => return Some(Token::Word(buffer)),
                        };
                        if ch != ' ' && ch != '\n' {
                            buffer.push(ch);
                        } else {
                            self.push_back(ch);
                            return Some(Token::Word(buffer));
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::{TokenIterator, Token, Step, StepPhase, Choice, NodeName, Conditional, Node};
    use super::{Expr, Term, UnaryOp};
    use super::{parse_step, parse_node_contents, parse_node, parse_nodes, parse_expr};

    #[test]
    fn tokenize_number() {
        let input = "-1.23";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Minus);
        assert_eq!(t.next().unwrap(), Token::Number(1.23));
        assert!(t.next().is_none());
    }

    #[test]
    fn tokenize_numbers() {
        let input = "-123.232 55 21.4 0.2";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Minus);
        assert_eq!(t.next().unwrap(), Token::Number(123.232));
        assert_eq!(t.next().unwrap(), Token::Number(55.));
        assert_eq!(t.next().unwrap(), Token::Number(21.4));
        assert_eq!(t.next().unwrap(), Token::Number(0.2));
        assert!(t.next().is_none());
    }

    #[test]
    fn tokenize_word() {
        let input = "hello";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Word("hello".to_string()));
        assert!(t.next().is_none());
    }

    #[test]
    fn tokenize_words() {
        let input = "why hello there";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Word("why".to_string()));
        assert_eq!(t.next().unwrap(), Token::Word("hello".to_string()));
        assert_eq!(t.next().unwrap(), Token::Word("there".to_string()));
        assert!(t.next().is_none());
    }

    #[test]
    fn tokenize_words_and_numbers_on_lines() {
        let input = "why\n5\nhello \n-0.1there";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Word("why".to_string()));
        assert_eq!(t.next().unwrap(), Token::Number(5.));
        assert_eq!(t.next().unwrap(), Token::Word("hello".to_string()));
        assert_eq!(t.next().unwrap(), Token::Minus);
        assert_eq!(t.next().unwrap(), Token::Number(0.1));
        assert_eq!(t.next().unwrap(), Token::Word("there".to_string()));
        assert!(t.next().is_none());
    }

    #[test]
    fn track_indent() {
        let input = "     hi there\n    bye\n";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Word("hi".to_string()));
        assert_eq!(t.last_indent(), 5);
        assert_eq!(t.next().unwrap(), Token::Word("there".to_string()));
        assert_eq!(t.last_indent(), 5);
        assert_eq!(t.next().unwrap(), Token::Word("bye".to_string()));
        assert_eq!(t.last_indent(), 4);
        assert!(t.next().is_none());
    }

    #[test]
    fn remainder_of_line() {
        let input = "     hi there\n    bye  bye";
        let mut t = TokenIterator::new(input);
        assert_eq!(t.next().unwrap(), Token::Word("hi".to_string()));
        assert_eq!(t.remainder_of_line(), Some(" there".to_string()));
        assert_eq!(t.next().unwrap(), Token::Word("bye".to_string()));
        assert_eq!(t.remainder_of_line(), Some("  bye".to_string()));
        assert!(t.remainder_of_line().is_none());
        assert!(t.next().is_none());
    }

    #[test]
    fn parse_dialogue_with_option() {
        let input = "this is dialogue\n[[this is a choice|targetnode]]";
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        assert_eq!(step, Step::Dialogue("this is dialogue".to_string(),
                                        vec![Choice {
                                            text: "this is a choice".to_string(),
                                            target: NodeName("targetnode".to_string()),
                                            condition: None
                                        }]));
    }

    #[test]
    fn parse_dialogue_with_two_options() {
        let input = "this is dialogue\n[[this is a choice|targetnode]]\n[[this is another choice|targetnode2]]";
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        assert_eq!(step, Step::Dialogue("this is dialogue".to_string(),
                                        vec![Choice {
                                            text: "this is a choice".to_string(),
                                            target: NodeName("targetnode".to_string()),
                                            condition: None
                                        },
                                        Choice {
                                            text: "this is another choice".to_string(),
                                            target: NodeName("targetnode2".to_string()),
                                            condition: None
                                        }]));
    }

    #[test]
    fn parse_conditional_dialogue() {
        let input = "<<if true>>\nthis is dialogue\n[[this is a choice|targetnode]]\n<<endif>>";
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        let expected = Step::Conditional(
            Conditional::Tmp("true".to_string()),
            vec![Step::Dialogue(
                "this is dialogue".to_string(),
                vec![Choice {
                    text: "this is a choice".to_string(),
                    target: NodeName("targetnode".to_string()),
                    condition: None
                }])],
            vec![],
            vec![]
        );
        assert_eq!(step, expected);
    }

    #[test]
    fn parse_conditional_dialogue_with_else() {
        let input = r#"<<if true>>
this is dialogue
[[this is a choice|targetnode]]
<<else>>
this is other dialogue
[[this is another choice|targetnode2]]
<<endif>>"#;
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        let expected = Step::Conditional(
            Conditional::Tmp("true".to_string()),
            vec![Step::Dialogue(
                "this is dialogue".to_string(),
                vec![Choice {
                    text: "this is a choice".to_string(),
                    target: NodeName("targetnode".to_string()),
                    condition: None
                }])],
            vec![],
            vec![Step::Dialogue(
                "this is other dialogue".to_string(),
                vec![Choice {
                    text: "this is another choice".to_string(),
                    target: NodeName("targetnode2".to_string()),
                    condition: None
                }])]
        );
        assert_eq!(step, expected);
    }

    #[test]
    fn parse_conditional_dialogue_with_else_if() {
        let input = r#"<<if true>>
this is dialogue
[[this is a choice|targetnode]]
<<elseif false>>
this is other dialogue
[[this is another choice|targetnode2]]
<<elseif true>>
third dialogue!
<<else>>
whatever
[[look a choice|targetnode3]]
<<endif>>"#;
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        let expected = Step::Conditional(
            Conditional::Tmp("true".to_string()),
            vec![Step::Dialogue(
                "this is dialogue".to_string(),
                vec![Choice {
                    text: "this is a choice".to_string(),
                    target: NodeName("targetnode".to_string()),
                    condition: None
                }])
            ],
            vec![(Conditional::Tmp("false".to_string()),
                  vec![Step::Dialogue(
                      "this is other dialogue".to_string(),
                      vec![Choice {
                          text: "this is another choice".to_string(),
                          target: NodeName("targetnode2".to_string()),
                          condition: None
                      }])
                  ]),
                 (Conditional::Tmp("true".to_string()),
                  vec![Step::Dialogue(
                      "third dialogue!".to_string(),
                      vec![])
                  ])
            ],
            vec![Step::Dialogue(
                "whatever".to_string(),
                vec![Choice {
                    text: "look a choice".to_string(),
                    target: NodeName("targetnode3".to_string()),
                    condition: None
                }])
            ]
        );
        assert_eq!(step, expected);
    }

    #[test]
    fn parse_command() {
        let input = "<<move doo to wop>>";
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        assert_eq!(step, Step::Command("move doo to wop".to_string()));
    }

    #[test]
    fn parse_commands() {
        let input = "<<move doo to wop>>\n<<hi>>";
        let mut t = TokenIterator::new(input);
        let step = parse_step(&mut t).unwrap();
        assert_eq!(step, Step::Command("move doo to wop".to_string()));
        let step = parse_step(&mut t).unwrap();
        assert_eq!(step, Step::Command("hi".to_string()));
    }

    #[test]
    fn parse_until_end_of_node() {
        let input = r#"dialogue
dialogue2
dialogue3
===
more
"#;
        let mut t = TokenIterator::new(input);
        let steps = parse_node_contents(&mut t).unwrap();
        let expected = vec![
            Step::Dialogue("dialogue".to_string(), vec![]),
            Step::Dialogue("dialogue2".to_string(), vec![]),
            Step::Dialogue("dialogue3".to_string(), vec![]),
        ];
        assert_eq!(steps, expected);
        assert_eq!(t.next().unwrap(), Token::Word("more".to_string()));
    }

    #[test]
    fn parse_whole_node() {
        let input = r#"title: whee hello
extra: hi there
---
dialogue
dialogue2
dialogue3
==="#;
        let mut t = TokenIterator::new(input);
        let mut extra = HashMap::new();
        extra.insert("extra".to_string(), "hi there".to_string());
        let expected = Node {
            title: NodeName("whee hello".to_string()),
            extra: extra,
            steps: vec![
                Step::Dialogue("dialogue".to_string(), vec![]),
                Step::Dialogue("dialogue2".to_string(), vec![]),
                Step::Dialogue("dialogue3".to_string(), vec![]),
            ],
            visited: false,
        };
        let node = parse_node(&mut t).unwrap();
        assert_eq!(node, expected);
    }

    #[test]
    fn parse_multiple_nodes() {
        let input = r#"title: whee hello
extra: hi there
---
dialogue
dialogue2
dialogue3
===


extra: foo bar -5
title: title!
---
dialogue
[[option|whee hello]]
[[option2|title!]]
===
"#;
        let mut t = TokenIterator::new(input);
        let mut extra = HashMap::new();
        extra.insert("extra".to_string(), "hi there".to_string());
        let mut extra2 = HashMap::new();
        extra2.insert("extra".to_string(), "foo bar -5".to_string());
        let expected = vec![
            Node {
                title: NodeName("whee hello".to_string()),
                extra: extra,
                steps: vec![
                    Step::Dialogue("dialogue".to_string(), vec![]),
                    Step::Dialogue("dialogue2".to_string(), vec![]),
                    Step::Dialogue("dialogue3".to_string(), vec![]),
                ],
                visited: false,
            },
            Node {
                title: NodeName("title!".to_string()),
                extra: extra2,
                steps: vec![
                    Step::Dialogue("dialogue".to_string(), vec![
                        Choice {
                            text: "option".to_string(),
                            target: NodeName("whee hello".to_string()),
                            condition: None,
                        },
                        Choice {
                            text: "option2".to_string(),
                            target: NodeName("title!".to_string()),
                            condition: None,
                        },
                    ]),
                ],
                visited: false,
            },
        ];

        let nodes = parse_nodes(&mut t).unwrap();
        assert_eq!(nodes, expected);
    }

    #[test]
    fn parse_number_expression() {
        let input = "5.4";
        let mut t = TokenIterator::new(input);
        assert_eq!(parse_expr(&mut t).unwrap(), Expr::Term(Term::Number(5.4)));
    }

    #[test]
    fn parse_negative_number_expression() {
        let input = "-5.4";
        let mut t = TokenIterator::new(input);
        let expected = Expr::Unary(UnaryOp::Negate, Box::new(Expr::Term(Term::Number(5.4))));
        assert_eq!(parse_expr(&mut t).unwrap(), expected);
    }
}
