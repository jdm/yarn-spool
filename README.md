This is a library for parsing and evaluating [Yarn Dialogue](https://github.com/thesecretlab/YarnSpinner/tree/master/Documentation/YarnSpinner-Dialogue) scripts in Rust games. It is designed to be embedded with game-provided hooks to affect the game state without making assumptions about how the dialogue will be displayed.

There are two main pieces:
1. the `YarnHandler` trait, which provides hooks for the dialogue engine to call back into the game
2. the `YarnEngine` type, which is responsible for parsing Yarn scripts and encapsulates the ongoing dialogue state

To begin a dialogue, call the `YarnEngine::activate` method. When it's time to move on from the current line of dialogue, call `YarnEngine::proceed`. If a choice is required in order to proceed, call `YarnEngine::choose` instead.

For an example of integrating yarn-spool into a game, look at the source of the [example game](examples/simple.rs).