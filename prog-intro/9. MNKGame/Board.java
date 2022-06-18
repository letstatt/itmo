package MNKGame;

public interface Board {
    Observer getObserver();
    CellManager getCellManager();
    Result makeMove(Move move, int playerId);
    Cell getCellByNumber(int no);

    interface CellManager {
        void changeCellTag(final Cell cell, final char c);
        Cell getDefaultCell();
        Cell getVoidCell();
        Cell getCellByNumber(int no);
        int getCellsCount();
        void setObserver(Observer observer);
        char toChar(Cell cell);
    }

    interface Observer {
        Cell getCell(int r, int c);
        boolean isValid(Move move);
        int getHeight();
        int getWidth();
        int getK();
        int playersCount();
        String toString(Cell cell);
    }
}
