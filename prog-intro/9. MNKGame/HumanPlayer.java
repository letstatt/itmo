package MNKGame;

import util.CustomScanner;

import java.io.PrintStream;
import java.util.NoSuchElementException;

public class HumanPlayer implements Player {
    private final PrintStream out;
    private final CustomScanner in;
    private final String name;

    public HumanPlayer(final PrintStream out, final CustomScanner in, final String name) {
        this.out = out;
        this.in = in;
        this.name = name;
    }

    public HumanPlayer(String name) {
        this(System.out, new CustomScanner(System.in), name);
    }

    @Override
    public Move move(final Board.Observer observer, final Cell cell) {
        out.println("\nCurrent board:");
        out.println(observer.toString(cell));
        out.printf("\n%s's move\n", name);
        out.println("Enter row and column");

        while (true) {
            try {
                final Move move = new Move(in.nextInt(), in.nextInt(), cell);
                in.skipEOL();

                if (observer.isValid(move)) {
                    return move;
                }
                out.println("Move " + move + " is invalid, try again");

            } catch (NoSuchElementException | NumberFormatException e) {
                if (!in.hasNextLine()) break;
                out.println("You should enter two integers, try again");
                in.skipLine();
            }
        }

        out.printf("Player %s has disconnected.\n", name);
        return null;
    }
}
