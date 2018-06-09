use parse;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NodeName(pub String);
#[derive(Debug, PartialEq)]
pub(crate) struct VariableName(pub String);

#[derive(Debug, PartialEq)]
pub(crate) struct Choice {
    text: String,
    kind: ChoiceKind,
}

impl Choice {
    pub(crate) fn external(text: String, name: NodeName) -> Choice {
        Choice {
            text: text,
            kind: ChoiceKind::External(name),
        }
    }

    pub(crate) fn inline(text: String, steps: Vec<Step>, condition: Option<Expr>) -> Choice {
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
pub(crate) enum Step {
    Dialogue(String, Vec<Choice>),
    Command(String),
    Assign(VariableName, Expr),
    Conditional(Expr, Vec<Step>, Vec<(Expr, Vec<Step>)>, Vec<Step>),
    Jump(NodeName),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Term(Term),
    Parentheses(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub(crate) enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, PartialEq)]
pub(crate) enum BinaryOp {
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
pub(crate) enum Term {
    Number(f32),
    Boolean(bool),
    String(String),
    Variable(VariableName),
    Function(String, Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Node {
    pub title: NodeName,
    pub extra: HashMap<String, String>,
    pub steps: Vec<Step>,
    pub visited: bool,
}

pub struct YarnEngine {
    nodes: HashMap<NodeName, Node>,
}

impl YarnEngine {
    pub fn new() -> YarnEngine {
        YarnEngine {
            nodes: HashMap::new(),
        }
    }

    pub fn load_from_string(&mut self, s: &str) -> Result<(), ()> {
        let nodes = parse::parse_nodes_from_string(s)?;
        for node in nodes {
            self.nodes.insert(node.title.clone(), node);
        }
        Ok(())
    }
}
