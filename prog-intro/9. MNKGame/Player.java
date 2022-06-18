package MNKGame;

public interface Player {
    Move move(final Board.Observer observer, final Cell cell);
}
