package MNKGame;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random;

    public RandomPlayer(final Random random) {
        this.random = random;
    }

    public RandomPlayer() {
        this(new Random());
    }

    @Override
    public Move move(final Board.Observer observer, final Cell cell) {
        while (true) {
            int r = random.nextInt(observer.getHeight());
            int c = random.nextInt(observer.getWidth());
            final Move move = new Move(r, c, cell);
            if (observer.isValid(move)) {
                return move;
            }
        }
    }
}
