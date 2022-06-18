package MNKGame;


import util.CustomScanner;

import java.io.IOException;
import java.io.PrintStream;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public class Main {
    public static PrintStream out;
    public static CustomScanner in;

    public static void main(String[] args) {
        in = new CustomScanner(System.in);
        out = System.out;

        out.println("Welcome to MNK Game, let's setup your match!");

        final boolean rhombus;
        final Board board;
        final Player[] players;

        try {
            rhombus = useRhombusBoard();
            board = getBoard(rhombus);
            players = getPlayers(board.getObserver().playersCount());

        } catch (IllegalStateException e) {
            out.println("Unknown error during game init");
            System.exit(1);
            return; // suppress warning "players might not be initialized"
        }

        out.println("\nOK, let's go!");

        boolean hasHuman = false;
        for (Player i: players) {
            if (i.getClass().isAssignableFrom(HumanPlayer.class)) {
                hasHuman = true;
            }
        }

        if (!hasHuman) {
            out.println("We don't have humans, the fight will be speedy!");
        }

        final Game game = new Game(players);
        int result = game.play(board);

        out.println("\nFinal board state:");
        out.println(board.getObserver().toString(null));
        out.println();

        if (result == 0) {
            out.println("Round was a draw!");
        } else if (result == 1) {
            out.println("Game cannot be continued, shutting down...");
        } else if (result > 0) {
            result = result / 2;
            out.printf("Player number %d has won\n", result);
        } else {
            result = -result / 2;
            out.printf("Player number %d has lost trying to break the rules\n", result);
        }
    }

    public static boolean useRhombusBoard() {
        while (in.hasNextLine()) {
            out.print("Use square board or rhombus one? [S/R]: ");
            String s = in.next();

            if (in.hasNext()) {
                out.println("Expected one symbol, try again");
                in.skipLine();
                continue;
            }

            in.skipEOL();

            if (s.length() == 0) {
                out.println("Incorrect input, try again");
                continue;
            }

            char c = Character.toLowerCase(s.charAt(0));

            if (c != 's' && c != 'r') {
                out.println("Incorrect input, try again");
            } else {
                return c == 'r';
            }
        }

        throw new IllegalStateException("Standard input is closed");
    }

    public static Board getBoard(boolean rhombus) {
        out.println("\nOK, now type 4 positive integers - M,N,K,P parameters.");
        out.println("It is width, height, the condition of winning, count of players respectively.");
        out.println("Example: 3 3 3 2\n");

        while (in.hasNextLine()) {
            try {
                out.print("Waiting for input: ");
                int m = in.nextInt();
                int n = in.nextInt();
                int k = in.nextInt();
                int p = in.nextInt();

                if (in.hasNext()) {
                    out.println("Too many parameters, try again");
                    in.skipLine();
                    continue;
                }

                in.skipEOL();

                try {
                    return (rhombus
                            ? new RhombusBoard(m, n, k, p)
                            : new MNKBoard(m, n, k, p)
                    );

                } catch (IllegalArgumentException e) {
                    out.print("An error occurred during creating the board: ");
                    out.println(e.getMessage());
                }

            } catch (NoSuchElementException e) {
                out.println("Incorrect input, try again");
                in.skipLine();
            }
        }

        throw new IllegalStateException("Standard input is closed");
    }

    public static Player[] getPlayers(int players) {
        out.println("\nLast step is naming your players.");
        out.println("There are 3 types of players - Human, Random and Sequential.");
        out.println("To define the order write them one by one in this way:");
        out.println("* If Human type his name");
        out.println("* If Random type 0");
        out.println("* Otherwise type 1\n");
        out.printf("Note: you need to create %d players.\n", players);
        out.println("Example: 0 0 GK 1 YS\n");

        String[] names = new String[players];

        while (in.hasNextLine()) {
            try {
                out.print("Waiting for input: ");

                for (int i = 0; i < players; ++i) {
                    names[i] = in.next();

                    if (names[i].length() == 0) {
                        throw new NoSuchElementException();
                    }
                }

                if (in.hasNext()) {
                    out.println("Too many players, try again");
                    in.skipLine();
                    continue;
                }

                in.skipEOL();
                Player[] p = new Player[players];

                for (int i = 0; i < players; ++i) {
                    switch(names[i]) {
                        case "0":
                            p[i] = new RandomPlayer();
                            break;
                        case "1":
                            p[i] = new SequentialPlayer();
                            break;
                        default:
                            p[i] = new HumanPlayer(names[i]);
                    }
                }

                return p;

            } catch (NoSuchElementException e) {
                out.println("Too few players, try again");
                in.skipLine();
            }
        }

        throw new IllegalStateException("Standard input is closed");
    }
}
