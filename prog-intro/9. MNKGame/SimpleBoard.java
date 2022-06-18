package MNKGame;

import java.util.*;

public abstract class SimpleBoard implements Board {

    private final CellManager cellManager;
    private Observer observer = null;
    protected final Cell[][] cells;

    public SimpleBoard(final int m, final int n, final int k, final int p, final CellManager manager) {
        if (manager.getCellsCount() < p) {
            throw new IllegalArgumentException("Too many players");
        }
        if (k > Math.max(m, n)) {
            throw new IllegalArgumentException("Board cannot be solved");
        }
        if (n < 1 || m < 1 || k < 1 || p < 1) {
            throw new IllegalArgumentException("M,N,K,P cannot be less or equal zero");
        }

        cells = new Cell[n][m];
        cellManager = manager;
        initBoard(n, m);
    }

    protected void initBoard(int n, int m) {
        for (Cell[] i: cells) {
            Arrays.fill(i, getCellManager().getDefaultCell());
        }
    }

    @Override
    public final Observer getObserver() {
        if (observer == null) {
            throw new UnsupportedOperationException("Observer cannot be extracted");
        }
        return observer;
    }

    protected final void setObserver(final Observer obs) {
        cellManager.setObserver(obs);
        observer = obs;
    }

    @Override
    public final CellManager getCellManager() {
        return cellManager;
    }

    @Override
    public Cell getCellByNumber(int no) {
        return cellManager.getCellByNumber(no);
    }

    public String toString(final Cell cell) {
        if (observer == null) {
            return null;
        }

        int rulerHeightPad = (int) Math.ceil(Math.log10(observer.getHeight())) + 1;
        int rulerWidthPad = (int) Math.ceil(Math.log10(observer.getWidth()));
        final StringBuilder sb = new StringBuilder(" ".repeat(rulerHeightPad + 2));

        for (int r = 0, acc = 10, pad = rulerWidthPad; r < observer.getWidth(); ++r) {
            if (r == acc) {
                acc *= 10;
                pad -= 1;
            }
            sb.append(" ".repeat(pad));
            sb.append(r);
        }

        sb.append("\n");
        sb.append(" ".repeat(rulerHeightPad + 2));
        sb.append("-".repeat((rulerWidthPad + 1) * observer.getWidth()));

        for (int r = 0, acc1 = 10, pad1 = rulerHeightPad - 1; r < observer.getHeight(); ++r) {
            if (r == acc1) {
                acc1 *= 10;
                pad1 -= 1;
            }
            sb.append("\n");
            sb.append(" ".repeat(pad1));
            sb.append(r);
            sb.append(" |");

            for (int c = 0; c < cells[r].length; ++c) {
                sb.append(" ".repeat(rulerWidthPad));
                char cur = cellManager.toChar(cells[r][c]);

                if (cells[r][c].equals(cell)) {
                    sb.append("\u001B[34m");
                    sb.append(cur);
                    sb.append("\u001B[0m");
                } else {
                    sb.append(cur);
                }
            }
        }
        return sb.toString();
    }

    protected static class CellManagerImpl implements CellManager {
        private final Map<Cell, Character> allowed = new LinkedHashMap<>();
        private Observer observer = null;

        public CellManagerImpl() {
            for (Cell i: Cell.values()) {
                if (i != getDefaultCell() && i != getVoidCell()) {
                    allowed.put(i, i.toString().charAt(0));
                }
            }
        }

        @Override
        public void changeCellTag(final Cell cell, final char c) {
            allowed.replace(cell, c);
        }

        @Override
        public int getCellsCount() {
            return allowed.size();
        }

        public void setObserver(final Observer obs) {
            observer = obs;
        }

        @Override
        public Cell getDefaultCell() {
            return Cell.Default;
        }

        @Override
        public Cell getVoidCell() {
            return Cell.Void;
        }

        @Override
        public Cell getCellByNumber(int no) {
            if (0 <= no && no < observer.playersCount()) {
                for (Map.Entry<Cell, Character> i: allowed.entrySet()) {
                    if (no == 0) return i.getKey();
                    no -= 1;
                }
            }

            throw new RuntimeException("Weird behavior: there are no next cells");
        }

        @Override
        public char toChar(Cell cell) {
            if (cell == getDefaultCell()) {
                return '.';
            } else if (cell == getVoidCell()) {
                return ' ';
            }
            return allowed.get(cell);
        }
    }

    protected abstract class ObserverImpl implements Observer {
        private final int m, n, k, p;

        public ObserverImpl(final int m, final int n, final int k, final int p) {
            this.m = m;
            this.n = n;
            this.k = k;
            this.p = p;
        }

        public Cell getCell(final int r, final int c) {
            return cells[r][c];
        }

        public int getHeight() {
            return n;
        }

        public int getWidth() {
            return m;
        }

        public int getK() {
            return k;
        }

        public int playersCount() {
            return p;
        }

        public String toString(Cell cell) {
            return SimpleBoard.this.toString(cell);
        }
    }
}
