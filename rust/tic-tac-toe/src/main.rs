use std::fmt::{Display, Formatter};
use std::io::{BufRead, Stdin, Stdout, Write};

#[derive(PartialEq, Clone, Copy)]
enum Cell {
    Empty,
    X,
    O,
}

impl Cell {
    fn inverse(&self) -> Cell {
        match *self {
            Cell::Empty => Cell::Empty,
            Cell::X => Cell::O,
            Cell::O => Cell::X,
        }
    }
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match *self {
            Self::Empty => write!(f, " "),
            Self::X => write!(f, "X"),
            Self::O => write!(f, "O"),
        }
    }
}

#[derive(PartialEq)]
enum Turn {
    Player,
    Computer,
}

impl Turn {
    fn inverse(&self) -> Turn {
        match *self {
            Turn::Computer => Turn::Player,
            Turn::Player => Turn::Computer,
        }
    }
}

static WIN_SEQUENCES: [(usize, usize, usize); 8] = [
    // rows
    (0, 1, 2),
    (3, 4, 5),
    (6, 7, 8),
    // columns
    (0, 3, 6),
    (1, 4, 7),
    (2, 5, 8),
    // diagonals
    (0, 4, 8),
    (2, 4, 6),
];

fn get_winner(table: &[Cell; 9]) -> Cell {
    for (i, j, k) in WIN_SEQUENCES {
        if (table[i] == table[j]) && (table[i] == table[k]) && table[i] != Cell::Empty {
            return table[i];
        }
    }
    Cell::Empty
}

fn readline(stdin: &Stdin) -> String {
    let mut lock = stdin.lock();
    let mut line = String::new();
    lock.read_line(&mut line).unwrap();
    String::from(line.trim())
}

fn startup(stdin: &Stdin, mut stdout: &Stdout) -> Cell {
    println!("Welcome to tic-tae-toe.");
    println!("Choose your mark! (X goes first)");
    print!("Enter X or O: ");
    stdout.flush().unwrap();

    return loop {
        let line = readline(stdin);
        match line.as_str() {
            "X" => break Cell::X,
            "O" => break Cell::O,
            _ => {
                print!("Try again: ");
                stdout.flush().unwrap();
                continue;
            }
        }
    };
}

fn print_table(table: &[Cell; 9]) {
    println!("+---+---+---+");
    for i in 0..3 {
        let start_ind = i * 3;
        println!(
            "| {} | {} | {} |",
            table[start_ind],
            table[start_ind + 1],
            table[start_ind + 2],
        );
        println!("+---+---+---+");
    }
    println!();
}

fn ask_player(stdin: &Stdin, mut stdout: &Stdout, table: &[Cell; 9]) -> usize {
    loop {
        if let Some(pos) = (|| -> Option<usize> {
            print!("Please, choose coords (row column): ");
            stdout.flush().unwrap();

            let line = readline(stdin);
            let mut parts = line.split(' ');
            let row: usize = parts.next()?.trim().parse().ok()?;
            let col: usize = parts.next()?.trim().parse().ok()?;
            Some(row * 3 + col)
        })() {
            if let Some(Cell::Empty) = table.get(pos) {
                break pos;
            }
        }
    }
}

fn get_score(me: Cell, table: &[Cell; 9]) -> i32 {
    let winner = get_winner(table);
    if winner == Cell::Empty {
        0
    } else if winner == me {
        10
    } else {
        -10
    }
}

fn minimax(me: Cell, my_turn: bool, table: &mut [Cell; 9]) -> (i32, Option<usize>) {
    let score = get_score(me, table);
    if score != 0 {
        return (score, None);
    }
    match table
        .iter()
        .enumerate()
        .filter(|(_, &cell)| cell == Cell::Empty)
        .map(|(pos, _)| pos)
        .collect::<Vec<usize>>()
        .iter()
        .map(|&pos| {
            table[pos] = if my_turn { me } else { me.inverse() };
            let (val, _) = minimax(me, !my_turn, table);
            table[pos] = Cell::Empty;
            (val, Some(pos))
        })
        .reduce(if my_turn {
            std::cmp::max
        } else {
            std::cmp::min
        }) {
        None => (0, None),
        Some(val) => val,
    }
}

fn ask_computer(mark: Cell, table: &mut [Cell; 9]) -> usize {
    minimax(mark, true, table).1.unwrap()
}

fn main() {
    // get console i/o handles
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    // setup game
    let player = startup(&stdin, &stdout);
    let computer = player.inverse();
    let mut turn = match player {
        Cell::X => Turn::Player,
        _ => Turn::Computer,
    };

    let mut winner = Cell::Empty;
    let mut table = [Cell::Empty; 9];
    let mut turns = 0;

    println!("\nThe game begins!");
    if turn == Turn::Player {
        print_table(&table);
    }

    while winner == Cell::Empty && turns < table.len() {
        let decision = match turn {
            Turn::Player => {
                println!("Your turn, {}.", player);
                (ask_player(&stdin, &stdout, &table), player)
            }
            Turn::Computer => {
                println!("Let computer decide...");
                (ask_computer(computer, &mut table), computer)
            }
        };

        table[decision.0] = decision.1;
        winner = get_winner(&table);
        turn = turn.inverse();
        turns += 1;

        print_table(&table);
    }

    if winner == Cell::Empty {
        println!("Appears to be a draw!");
    } else {
        println!(
            "{}",
            if winner == player {
                "Congratulations, you win!"
            } else {
                "Computer wins!"
            }
        );
    }
    println!();
}
