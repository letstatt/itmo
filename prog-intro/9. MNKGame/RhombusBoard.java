package MNKGame;

import java.util.Arrays;

public class RhombusBoard extends MNKBoard {

    public RhombusBoard(int m, int n, int k, int p) {
        super(2 * m - 1, 2 * n - 1, k, p);
        free = n * m;
    }

    @Override
    protected void initBoard(int n, int m) {
        if (m != n) {
            throw new IllegalArgumentException("Sides must be equal");
        }

        CellManager cm = getCellManager();

        for (Cell[] i: cells) {
            Arrays.fill(i, cm.getVoidCell());
        }

        int mid = n / 2;
        int i = 0;

        while(mid - i >= 0) {
            for (int j = i; j < m - i; ++j) {
                cells[mid - i][j] = cm.getDefaultCell();
                cells[mid + i][j] = cm.getDefaultCell();
            }
            i += 1;
        }
    }
}
