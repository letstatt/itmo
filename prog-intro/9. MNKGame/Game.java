package MNKGame;

public class Game {
    private final Player[] players;

    public Game(final Player ...players) {
        this.players = players;
    }

    public int play(final Board board) {
        if (board.getObserver().playersCount() != players.length) {
            throw new IllegalArgumentException("Players counts mismatch");
        }

        while (true) {
            for (int i = 0; i < players.length; ++i) {
                final int result = move(board, players[i], i);
                if (result != -1) {
                    return result;
                }
            }
        }
    }

    private int move(final Board board, final Player player, final int no) {
        final Move move = player.move(board.getObserver(), board.getCellByNumber(no));
        if (move == null) {
            return 1;
        }

        final Result result = board.makeMove(move, no);

        if (result == Result.WIN) {
            return (1 + no) * 2;
        } else if (result == Result.LOSE) {
            return (-1 - no) * 2;
        } else if (result == Result.DRAW) {
            return 0;
        } else {
            return -1;
        }
    }
}
