use parse;
use std::collections::HashMap;

//TODO: dialogue options inside conditionals

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeName(pub String);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableName(pub String);

pub struct Variables(HashMap<VariableName, Value>);
impl Variables {
    fn set(&mut self, name: VariableName, value: Value) {
        self.0.insert(name, value);
    }
}

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

struct Conversation {
    node: NodeName,
    base_index: usize,
    indexes: Vec<StepIndex>,
}

impl Conversation {
    fn new(node: NodeName) -> Conversation {
        Conversation {
            node,
            base_index: 0,
            indexes: vec![],
        }
    }

    fn reset(&mut self, name: NodeName) {
        *self = Conversation::new(name);
    }
}

#[derive(Copy, Clone)]
enum StepIndex {
    Dialogue(usize, usize),
    If(usize),
    ElseIf(usize, usize),
    Else(usize),
}

impl StepIndex {
    fn advance(&mut self) {
        let idx = match *self {
            StepIndex::Dialogue(_, ref mut idx) |
            StepIndex::If(ref mut idx) |
            StepIndex::ElseIf(_, ref mut idx) |
            StepIndex::Else(ref mut idx) => idx,
        };
        *idx += 1;
    }
}

#[derive(PartialEq)]
enum ExecutionStatus {
    Continue,
    Halt,
}

pub enum Value {
    String(String),
    Number(f32),
    Boolean(bool),
}

impl Value {
    fn as_bool(&self) -> bool {
        match *self {
            Value::Boolean(b) => b,
            Value::String(ref s) => !s.is_empty(),
            Value::Number(f) => f != 0.0,
        }
    }
}

pub struct Function {
    _num_args: usize,
    callback: Box<FunctionCallback>,
}

pub type FunctionCallback = Fn(Vec<Value>, &YarnEngine) -> Result<Value, ()>;

pub struct YarnEngine {
    handler: Box<YarnHandler>,
    state: NodeState,
    engine_state: EngineState,
}

struct EngineState {
    variables: Variables,
    functions: HashMap<String, Function>,
}

impl EngineState {
    fn evaluate(&self, _expr: &Expr) -> Result<Value, ()> {
        Err(()) //TODO
    }
}

struct NodeState {
    nodes: HashMap<NodeName, Node>,
    conversation: Option<Conversation>,
}

impl NodeState {
    fn get_current_step(&mut self) -> (Conversation, &Vec<Step>, usize) {
        let conversation = self.conversation.take().expect("missing conversation");
        let mut steps = {
            let current = self.nodes.get(&conversation.node).expect("missing node");
            &current.steps
        };
        let mut current_step_index = conversation.base_index;

        for index in conversation.indexes.iter() {
            match (&steps[current_step_index], *index) {
                (&Step::Dialogue(_, ref choices), StepIndex::Dialogue(choice, step_index)) => {
                    let choice = &choices[choice];
                    match choice.kind {
                        ChoiceKind::Inline(ref choice_steps, _) => {
                            steps = choice_steps;
                            current_step_index = step_index;
                        }
                        ChoiceKind::External(..) => unreachable!(),
                    }
                }
                (&Step::Conditional(_, ref if_steps, ..), StepIndex::If(step_index)) => {
                    steps = if_steps;
                    current_step_index = step_index;
                }
                (&Step::Conditional(_, _, ref else_ifs, ..), StepIndex::ElseIf(index, step_index)) => {
                    steps = &else_ifs[index].1;
                    current_step_index = step_index;
                }
                (&Step::Conditional(_, _, _, ref else_steps), StepIndex::Else(step_index)) => {
                    steps = else_steps;
                    current_step_index = step_index;
                }
                _ => unreachable!(),
            }
        }

        (conversation, steps, current_step_index)
    }
}

impl YarnEngine {
    pub fn new(handler: Box<YarnHandler>) -> YarnEngine {
        let mut engine = YarnEngine {
            state: NodeState {
                nodes: HashMap::new(),
                conversation: None,
            },
            engine_state: EngineState {
                variables: Variables(HashMap::new()),
                functions: HashMap::new(),
            },
            handler,
        };
        engine.register_function("visited".to_string(), 1, Box::new(|args, engine| {
            match args[0] {
                Value::String(ref s) => {
                    engine
                        .state
                        .nodes
                        .get(&NodeName(s.to_string()))
                        .map(|node| Value::Boolean(node.visited)).ok_or(())
                }
                _ => return Err(())
            }
        }));
        engine
    }

    pub fn load_from_string(&mut self, s: &str) -> Result<(), ()> {
        let nodes = parse::parse_nodes_from_string(s)?;
        for node in nodes {
            self.state.nodes.insert(node.title.clone(), node);
        }
        Ok(())
    }

    pub fn register_function(
        &mut self,
        name: String,
        num_args: usize,
        callback: Box<FunctionCallback>,
    ) {
        self.engine_state.functions.insert(name, Function {
            _num_args: num_args,
            callback,
        });
    }

    pub fn set_variable(
        &mut self,
        name: VariableName,
        value: Value
    ) {
        self.engine_state.variables.set(name, value);
    }

    pub fn activate(&mut self, node: NodeName) {
        //TODO: mark visited
        self.state.conversation = Some(Conversation::new(node));
        self.proceed();
    }

    pub fn choose(&mut self, choice: usize) {
        let conversation = {
            let (mut conversation, steps, current_step_index) = self.state.get_current_step();
            match &steps[current_step_index] {
                &Step::Dialogue(_, ref choices) => {
                    match choices[choice].kind {
                        ChoiceKind::External(ref node) => conversation.reset((*node).clone()),
                        ChoiceKind::Inline(..) => {
                            conversation.indexes.push(StepIndex::Dialogue(choice, 0));
                        }
                    }
                }
                &Step::Command(..) | &Step::Assign(..) | &Step::Conditional(..) | &Step::Jump(..) =>
                    unreachable!(),
            }
            conversation
        };
        self.state.conversation = Some(conversation);
        self.proceed();
    }

    pub fn proceed(&mut self) {
        while self.proceed_one_step() == ExecutionStatus::Continue {
        }
    }

    fn do_proceed_one_step(&mut self) -> (Conversation, ExecutionStatus) {
        let (mut conversation, steps, current_step_index) = self.state.get_current_step();

        if current_step_index >= steps.len() {
            self.handler.end_conversation();
            return (conversation, ExecutionStatus::Halt);
        }

        let (advance, execution_status) = match steps[current_step_index] {
            Step::Dialogue(ref text, ref choices) => {
                if choices.is_empty() {
                    self.handler.say(text.clone());
                    (true, ExecutionStatus::Halt)
                } else {
                    //TODO: conditional options
                    self.handler.choose(text.clone(), choices.iter().map(|c| c.text.clone()).collect());
                    (false, ExecutionStatus::Halt)
                }
            }
            Step::Command(ref command) => {
                self.handler.command(command.clone()).unwrap();
                (true, ExecutionStatus::Continue)
            }
            Step::Assign(ref name, ref expr) => {
                let value = self.engine_state.evaluate(expr).unwrap();
                self.engine_state.variables.set((*name).clone(), value);
                (true, ExecutionStatus::Continue)
            }
            Step::Conditional(ref expr, ref _if_steps, ref else_ifs, ref _else_steps) => {
                let value = self.engine_state.evaluate(expr).unwrap();
                if value.as_bool() {
                    conversation.indexes.push(StepIndex::If(0));
                } else {
                    let mut matched = false;
                    for (else_if_index, else_ifs) in else_ifs.iter().enumerate() {
                        let value = self.engine_state.evaluate(&else_ifs.0).unwrap();
                        if value.as_bool() {
                            conversation.indexes.push(StepIndex::ElseIf(else_if_index, 0));
                            matched = true;
                            break;
                        }
                    }
                    if !matched {
                        conversation.indexes.push(StepIndex::Else(0));
                    }
                }
                (false, ExecutionStatus::Continue)
            }
            Step::Jump(ref name) => {
                //TODO: mark visited
                conversation.reset((*name).clone());
                (false, ExecutionStatus::Continue)
            }
        };

        if advance {
            match conversation.indexes.last_mut() {
                Some(index) => index.advance(),
                None => conversation.base_index += 1,
            }
        }

        (conversation, execution_status)
    }

    fn proceed_one_step(&mut self) -> ExecutionStatus {
        let (conversation, execution_status) = self.do_proceed_one_step();

        self.state.conversation = Some(conversation);

        execution_status
    }
}

pub trait YarnHandler {
    fn say(&mut self, text: String);
    fn choose(&mut self, text: String, choices: Vec<String>);
    fn command(&mut self, action: String) -> Result<(), ()>;
    fn end_conversation(&mut self);
}
