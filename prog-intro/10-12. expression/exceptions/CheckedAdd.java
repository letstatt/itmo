package expression.exceptions;

import expression.AbstractOperator;
import expression.Add;

public class CheckedAdd extends Add {
    public CheckedAdd(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new UnsupportedOperationException("Doubles are not safe");
    }

    @Override
    protected int internalCalc(int x, int y) {
        int z = x + y; // Плотников: нельзя сначала вычислять, а потом делать проверку, иначе Java-машина взорвется!
        if (y > 0 && x > 0 && z <= 0 || y < 0 && x < 0 && z >= 0) { // какой кринж.
            throw new OverflowException();
        }
        return z;
    }
}
