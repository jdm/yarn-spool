use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct NodeName(String);
#[derive(Debug, PartialEq)]
struct VariableName(String);

#[derive(Debug, PartialEq)]
struct Choice {
    text: String,
    kind: ChoiceKind,
}

impl Choice {
    fn external(text: String, name: NodeName) -> Choice {
        Choice {
            text: text,
            kind: ChoiceKind::External(name),
        }
    }

    fn inline(text: String, steps: Vec<Step>, condition: Option<Expr>) -> Choice {
        Choice {
            text: text,
            kind: ChoiceKind::Inline(steps, condition),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ChoiceKind {
    External(NodeName),
    Inline(Vec<Step>, Option<Expr>),
}

#[derive(Debug, PartialEq)]
enum Step {
    Dialogue(String, Vec<Choice>),
    Command(String),
    Assign(VariableName, Expr),
    Conditional(Expr, Vec<Step>, Vec<(Expr, Vec<Step>)>, Vec<Step>),
    Jump(NodeName),
}

#[derive(Debug, PartialEq)]
enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Term(Term),
    Parentheses(Box<Expr>),
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
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
}

#[derive(Debug, PartialEq)]
enum Term {
    Number(f32),
    Boolean(bool),
    String(String),
    Variable(VariableName),
    #[allow(unused)]
    Function(String, Vec<Expr>), //TODO
}

#[derive(Debug, PartialEq)]
pub struct Node {
    title: NodeName,
    extra: HashMap<String, String>,
    steps: Vec<Step>,
    visited: bool,
}

fn parse_expr(tokenizer: &mut TokenIterator) -> Result<Expr, ()> {
    let t = tokenizer.next().ok_or(())?;
    let left = match t {
        Token::Number(num) => Expr::Term(Term::Number(num)),
        Token::ExclamationMark => {
            let expr = parse_expr(tokenizer)?;
            Expr::Unary(UnaryOp::Not, Box::new(expr))
        }
        Token::Word(ref w) if w == "not" => {
            let expr = parse_expr(tokenizer)?;
            Expr::Unary(UnaryOp::Not, Box::new(expr))
        }
        Token::Word(ref w) if w == "true" => {
            Expr::Term(Term::Boolean(true))
        }
        Token::Word(ref w) if w == "false" => {
            Expr::Term(Term::Boolean(false))
        }
        Token::Quote => {
            Expr::Term(Term::String(parse_string_until(tokenizer, '"')?))
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
        Token::LeftParenthesis => {
            Expr::Parentheses(Box::new(parse_expr(tokenizer)?))
        }
        _ => return Err(()),
    };
    match tokenizer.peek() {
        Some(')') | None => return Ok(left),
        _ => (),
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
        Token::LeftAngle => {
            if tokenizer.peek() == Some('=') {
                let _ = tokenizer.next();
                BinaryOp::LessThanEqual
            } else {
                BinaryOp::LessThan
            }
        }
        Token::RightAngle => {
            if tokenizer.peek() == Some('=') {
                let _ = tokenizer.next();
                BinaryOp::GreaterThanEqual
            } else {
                BinaryOp::GreaterThan
            }
        }
        Token::Word(word) => {
            match &*word {
                "and" => BinaryOp::And,
                "or" => BinaryOp::Or,
                "eq" | "is" => BinaryOp::Equals,
                "neq" => BinaryOp::NotEquals,
                "le" => BinaryOp::LessThan,
                "leq" => BinaryOp::LessThanEqual,
                "gt" => BinaryOp::GreaterThan,
                "geq" => BinaryOp::GreaterThanEqual,
                _ => return Err(()),
            }
        }
        _ => return Err(()),
    };
    let right = parse_expr(tokenizer)?;
    Ok(Expr::Binary(op, Box::new(left), Box::new(right)))
}

#[derive(Debug, PartialEq)]
enum Line {
    Dialogue(String),
    If(String),
    ElseIf(String),
    Else,
    EndIf,
    Action(String),
    Option(Option<String>, NodeName),
    InlineOption(String, Option<String>),
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
            let (text, cond) = match rest.find("<<") {
                Some(idx) => {
                    let remainder = &rest[idx + 2..].trim();
                    if !remainder.starts_with("if ") {
                        return Err(());
                    }
                    let end = remainder.find(">>").ok_or(())?;
                    (rest[..idx].trim().to_string(), Some(remainder[3..end].trim().to_string()))
                }
                None => (rest.trim().to_string(), None),
            };
            return Ok(Line::InlineOption(text, cond));
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

#[derive(Debug)]
enum DialogueOption {
    Inline(String, Option<String>),
    External(String, NodeName),
}

fn try_parse_option(tokenizer: &mut TokenIterator, indent: u32) -> Result<Option<DialogueOption>, ()> {
    let t = match tokenizer.peek() {
        Some(t) => t,
        None => return Ok(None),
    };
    if tokenizer.last_indent() < indent {
        return Ok(None);
    }
    if t == '[' || t == '-' {
        let (_indent, line) = parse_line(tokenizer)?;
        match line {
            Line::Option(Some(text), name) => Ok(Some(DialogueOption::External(text, name))),
            Line::InlineOption(s, condition) => Ok(Some(DialogueOption::Inline(s, condition))),
            _ => unreachable!(),
        }
    } else {
        Ok(None)
    }
}

struct ConditionalParts {
    if_steps: Vec<Step>,
    else_ifs: Vec<(Expr, Vec<Step>)>,
    else_steps: Vec<Step>,
}

#[derive(PartialEq)]
enum ConditionalParsePhase {
    If,
    ElseIf,
    Else,
}

fn parse_conditional(tokenizer: &mut TokenIterator, indent: u32) -> Result<ConditionalParts, ()> {
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
                let mut expr_tokenizer = TokenIterator::new(&s);
                let expr = parse_expr(&mut expr_tokenizer)?;
                parts.else_ifs.push((expr, vec![]));
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
                let step = parse_toplevel_line(tokenizer, l, indent)?;
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

fn parse_toplevel_line(tokenizer: &mut TokenIterator, line: Line, indent: u32) -> Result<Step, ()> {
    match line {
        Line::Dialogue(s) => {
            println!("found dialogue '{}'", s);
            let mut choices = vec![];
            loop {
                let opt = try_parse_option(tokenizer, indent)?;
                println!("found opt {:?} with indent {}", opt, indent);
                match opt {
                    Some(DialogueOption::Inline(text, condition)) => {
                        println!("peeking after inline opt: {:?}" ,tokenizer.peek());
                        let this_indent = tokenizer.last_indent();
                        println!("this indent: {}", this_indent);
                        let mut steps = vec![];
                        loop {
                            if tokenizer.peek().is_none() || tokenizer.last_indent() < this_indent {
                                break;
                            }
                            steps.push(parse_step(tokenizer)?);
                        }
                        let condition = match condition {
                            Some(c) => {
                                let mut expr_tokenizer = TokenIterator::new(&c);
                                Some(parse_expr(&mut expr_tokenizer)?)
                            }
                            None => None,
                        };
                        choices.push(Choice::inline(text, steps, condition));
                    }
                    Some(DialogueOption::External(text, node)) => {
                        choices.push(Choice::external(text, node));
                    }
                    None => break,
                }
            }
            return Ok(Step::Dialogue(s, choices));
        }
        Line::If(s) => {
            let mut expr_tokenizer = TokenIterator::new(&s);
            let expr = parse_expr(&mut expr_tokenizer)?;
            let parts = parse_conditional(tokenizer, indent)?;
            return Ok(Step::Conditional(expr,
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
        Line::Option(None, name) => {
            return Ok(Step::Jump(name));
        }
        Line::EndIf |
        Line::ElseIf(_) |
        Line::Else |
        Line::Option(..) |
        Line::InlineOption(..) =>
            return Err(())
    }
}


fn parse_step(tokenizer: &mut TokenIterator) -> Result<Step, ()> {
    let (indent, line) = parse_line(tokenizer)?;
    parse_toplevel_line(tokenizer, line, indent)
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

pub fn parse_nodes(tokenizer: &mut TokenIterator) -> Result<Vec<Node>, ()> {
    let mut nodes = vec![];
    while tokenizer.peek().is_some() {
        nodes.push(parse_node(tokenizer)?);
    }
    Ok(nodes)
}

#[derive(Debug, PartialEq)]
pub enum Token {
    DollarSign,
    LeftAngle,
    RightAngle,
    Equals,
    Pipe,
    Plus,
    Minus,
    Star,
    Slash,
    Quote,
    ExclamationMark,
    LeftBracket,
    RightBracket,
    LeftParenthesis,
    RightParenthesis,
    Number(f32),
    Word(String),
    StartNode,
}

pub struct TokenIterator<'a> {
    input: Box<Iterator<Item=char> + 'a>,
    last_char: Option<char>,
    last_indent: u32,
    start_of_line: bool,
}

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a str) -> TokenIterator<'a> {
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
            if self.start_of_line && ch == Some(' ') {
                self.last_indent += 1;
                continue;
            }
            self.start_of_line = false;
            if ch != Some('\n') {
                break;
            }
            self.start_of_line = true;
            self.last_indent = 0;
        }
        self.last_char = ch;
        ch
    }

    fn remainder_of_line(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(ch) = self.next_char() {
            if ch == '\n' {
                self.start_of_line = true;
                self.last_indent = 0;
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
            let ch = self.next_char()?;
            if ch != ' ' {
                self.start_of_line = false;
            }
            match ch {
                '(' => return Some(Token::LeftParenthesis),
                ')' => return Some(Token::RightParenthesis),
                '|' => return Some(Token::Pipe),
                '$' => return Some(Token::DollarSign),
                '<' => return Some(Token::LeftAngle),
                '>' => return Some(Token::RightAngle),
                '=' => return Some(Token::Equals),
                '-' => return Some(Token::Minus),
                '+' => return Some(Token::Plus),
                '*' => return Some(Token::Star),
                '/' => return Some(Token::Slash),
                '!' => return Some(Token::ExclamationMark),
                '"' => return Some(Token::Quote),
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
mod test;
