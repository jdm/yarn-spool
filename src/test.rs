use std::collections::HashMap;
use super::{TokenIterator, Token, Step, StepPhase, Choice, NodeName, Conditional, Node};
use super::{Expr, Term, UnaryOp, BinaryOp};
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

#[test]
fn parse_addition() {
    let input = "4 + 8";
    let mut t = TokenIterator::new(input);
    let expected = Expr::Binary(BinaryOp::Plus,
                                Box::new(Expr::Term(Term::Number(4.0))),
                                Box::new(Expr::Term(Term::Number(8.0))));
    assert_eq!(parse_expr(&mut t).unwrap(), expected);
}

#[test]
fn parse_parentheses() {
    let input = "(4 + 8)";
    let mut t = TokenIterator::new(input);
    let expected = Expr::Parentheses(
        Box::new(Expr::Binary(BinaryOp::Plus,
                              Box::new(Expr::Term(Term::Number(4.0))),
                              Box::new(Expr::Term(Term::Number(8.0))))));
    assert_eq!(parse_expr(&mut t).unwrap(), expected);
}

#[test]
fn parse_jump() {
    let input = "[[SomeNode.Walk]]";
    let mut t = TokenIterator::new(input);
    let step = parse_step(&mut t).unwrap();
    assert_eq!(step, Step::Jump(NodeName("SomeNode.Walk".to_string())));
}
