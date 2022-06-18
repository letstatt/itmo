package expression.exceptions;

import expression.AbstractOperator;
import expression.Divide;

public class CheckedDivide extends Divide {
    public CheckedDivide(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new UnsupportedOperationException("Doubles are not safe");
    }

    @Override
    protected int internalCalc(int x, int y) {
        if (y == 0) {
            throw new DivisionByZeroException();

        } else if (y == -1 && x == Integer.MIN_VALUE) {
            throw new OverflowException();
        }
        return x / y;
    }
}
