package MNKGame;

public class SequentialPlayer implements Player {
    @Override
    public Move move(final Board.Observer observer, final Cell cell) {
//        Board board = (Board) position;
//        board.makeMove()
        for (int r = 0; r < observer.getHeight(); ++r) {
            for (int c = 0; c < observer.getWidth(); ++c) {
                final Move move = new Move(r, c, cell);
                if (observer.isValid(move)) {
                    return move;
                }
            }
        }
        throw new IllegalStateException("No valid moves");
    }
}
