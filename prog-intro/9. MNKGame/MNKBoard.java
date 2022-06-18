package MNKGame;

public class MNKBoard extends SimpleBoard {

    protected int free;

    public MNKBoard(final int m, final int n, final int k, final int p) {
        super(m, n, k, p, new CellManagerImpl());
        this.free = n * m;

        CellManager cm = getCellManager();

        cm.changeCellTag(Cell.C, '-');
        cm.changeCellTag(Cell.D, '|');

        setObserver(new ObserverImpl(m, n, k, p) {
            @Override
            public boolean isValid(final Move move) {
                return 0 <= move.getRow() && move.getRow() < n
                        && 0 <= move.getColumn() && move.getColumn() < m
                        && getCell(move.getRow(), move.getColumn()) == getCellManager().getDefaultCell();
            }
        });
    }

    protected int expand(final Move move, final int di, final int dj) {
        final Observer observer = getObserver();
        final Cell c = move.getValue();
        int i = move.getRow();
        int j = move.getColumn();
        int res = 0;

        while (0 <= i && i < observer.getHeight() && 0 <= j && j < observer.getWidth() && observer.getCell(i, j) == c) {
            if (res < observer.getK()) {
                i += di;
                j += dj;
                res += 1;
                continue;
            }
            break;
        }
        return res;
    }

    @Override
    public Result makeMove(final Move move, final int playerId) {
        final Observer observer = getObserver();

        if (!getCellManager().getCellByNumber(playerId).equals(move.getValue()) || !observer.isValid(move)) {
            return Result.LOSE;
        }

        Cell c = move.getValue();
        int i = move.getRow();
        int j = move.getColumn();

        cells[i][j] = c;
        free -= 1;

        int row = expand(move, 0, -1) + expand(move, 0, 1);
        int diag1 = expand(move, -1, -1) + expand(move, 1, 1);
        int diag2 = expand(move, -1, 1) + expand(move, 1, -1);
        int col = expand(move, 1, 0) + expand(move, -1, 0);

        if (Math.max(Math.max(diag1, diag2), Math.max(row, col)) - 1 >= observer.getK()) {
            return Result.WIN;
        } else if (free == 0) {
            return Result.DRAW;
        }

        return Result.UNKNOWN;
    }
}
