package expression.exceptions;

import expression.AbstractOperator;
import expression.UnaryMinus;

public class CheckedNegate extends UnaryMinus {
    public CheckedNegate(AbstractOperator first) {
        super(first);
    }

    @Override
    protected double internalCalc(double x) {
        throw new UnsupportedOperationException("Doubles are not safe");
    }

    @Override
    protected int internalCalc(int x) {
        if (x == Integer.MIN_VALUE) {
            throw new OverflowException();
        }
        return -x;
    }
}
