package expression.exceptions;

import expression.AbstractOperator;
import expression.Multiply;

public class CheckedMultiply extends Multiply {
    public CheckedMultiply(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new UnsupportedOperationException("Doubles are not safe");
    }

    @Override
    protected int internalCalc(int x, int y) {
        int z = x * y;
        if (x != 0 && y != 0 && (z / x != y || z / y != x)) {
            throw new OverflowException();
        }
        return z;
    }
}
