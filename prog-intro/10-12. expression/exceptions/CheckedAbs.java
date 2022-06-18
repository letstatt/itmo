package expression.exceptions;

import expression.AbstractOperator;
import expression.UnaryOperator;
import expression.exceptions.OverflowException;

public class CheckedAbs extends UnaryOperator {

    public CheckedAbs(AbstractOperator first) {
        super(first);
    }

    @Override
    protected String getType() {
        return "abs";
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
        return (x < 0 ? -x : x);
    }
}
