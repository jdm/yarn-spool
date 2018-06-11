extern crate easycurses;
extern crate yarn_spool;

use easycurses::*;
use yarn_spool::{YarnEngine, YarnHandler, NodeName};
use std::cell::{RefCell, Cell};
use std::fs;
use std::iter::repeat;
use std::rc::Rc;
use std::thread::sleep;
use std::time::Duration;
use std::time::Instant;

#[derive(PartialEq)]
enum Phase {
    Dialogue(String),
    Game,
}

struct GameState {
    phase: RefCell<Phase>,
    draw_button: (Cell<Instant>, Cell<bool>),
}

struct Handler {
    state: Rc<GameState>,
}

impl YarnHandler for Handler {
    fn say(&mut self, text: String) {
        *self.state.phase.borrow_mut() = Phase::Dialogue(text);
    }

    fn choose(&mut self, _text: String, _choices: Vec<String>) {
    }

    fn command(&mut self, _action: String) -> Result<(), ()> {
        Ok(())
    }

    fn end_conversation(&mut self) {
        *self.state.phase.borrow_mut() = Phase::Game;
    }
}

fn main() {
    // Normal setup
    let mut easy = EasyCurses::initialize_system().unwrap();
    easy.set_cursor_visibility(CursorVisibility::Invisible);
    easy.set_echo(false);
    easy.set_keypad_enabled(true);
    easy.set_input_mode(InputMode::Character);
    easy.set_scrolling(false);
    easy.set_input_timeout(TimeoutMode::Immediate);

    let state = Rc::new(GameState {
        phase: RefCell::new(Phase::Game),
        draw_button: (Cell::new(Instant::now()), Cell::new(false)),
    });
    let handler = Handler { state: state.clone() };
    let mut engine = YarnEngine::new(Box::new(handler));

    let buffer = fs::read_to_string("examples/simple.yarn").unwrap();
    engine.load_from_string(&buffer).unwrap();

    // We need to know how wide our screen is.
    let (row_count, col_count) = easy.get_row_col_count();

    let frame_target_duration = Duration::new(1, 0).checked_div(60).expect("failed when rhs!=0, what?");

    let (mut x, mut y) = (col_count / 2, row_count / 2);
    let (dwarf_x, dwarf_y): (i32, i32) = (x - 5, y - 3);

    loop {
        let top_of_loop = Instant::now();
        // Gather/process any pending input
        while let Some(input) = easy.get_input() {
            let is_game = &*state.phase.borrow() == &Phase::Game;
            if is_game {
                let (xdiff, ydiff) = match input {
                    Input::KeyLeft => (-1, 0),
                    Input::KeyRight => (1, 0),
                    Input::KeyDown => (0, -1),
                    Input::KeyUp => (0, 1),
                    _ => (0, 0)
                };
                if xdiff != 0 || ydiff != 0 {
                    if x + xdiff == dwarf_x && y + ydiff == dwarf_y {
                        engine.activate(NodeName("dwarf".to_string()));
                    } else {
                        x = (x + xdiff).max(0).min(col_count - 1);
                        y = (y + ydiff).max(0).min(row_count - 1);
                    }
                }
            } else {
                if input == Input::Character('x') {
                    engine.proceed();
                }
            }
        }

        // Sleep a bit if we need to. This actually sleeps a little longer than
        // just the right time because it doesn't account for the display time
        // we'll use up after the sleep happens. However, curses doesn't really
        // demand perfect animation anyway.
        let elapsed_this_frame = top_of_loop.elapsed();
        if let Some(frame_remaining) = frame_target_duration.checked_sub(elapsed_this_frame) {
            sleep(frame_remaining);
        }

        // Display
        easy.bulk_insert_delete_line(row_count);

        // Dwarf
        easy.move_xy(dwarf_x, dwarf_y);
        easy.print_char('!');

        // Player
        easy.move_xy(x, y);
        easy.print_char('@');

        if let &Phase::Dialogue(ref s) = &*state.phase.borrow() {
            let mut draw_y = 4;
            let mut draw_x = col_count / 3;
            easy.move_xy(draw_x, draw_y);
            let border: String = repeat('#').take(col_count as usize / 3).collect();
            easy.print(&border);

            draw_y -= 1;
            easy.move_xy(draw_x, draw_y);
            easy.print_char('#');
            easy.move_xy(draw_x + 2, draw_y);
            easy.print(s);
            easy.move_xy(draw_x * 2 - 1, draw_y);
            easy.print_char('#');

            draw_y -= 1;
            easy.move_xy(draw_x, draw_y);
            easy.print_char('#');
            easy.move_xy(draw_x * 2  - 5, draw_y);
            if state.draw_button.1.get() {
                easy.print("(x)");
            } else {
                easy.print("   ");
            }

            easy.move_xy(draw_x * 2 - 1, draw_y);
            easy.print_char('#');

            draw_y -= 1;
            easy.move_xy(draw_x, draw_y);
            easy.print(&border);

            if state.draw_button.0.get().elapsed() > Duration::from_millis(500) {
                state.draw_button.0.set(Instant::now());
                state.draw_button.1.set(!state.draw_button.1.get());
            }
        }

        easy.refresh();
    }
}
