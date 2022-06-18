package expression.exceptions;

import expression.AbstractOperator;
import expression.UnaryOperator;

public class CheckedSqrt extends UnaryOperator {

    public CheckedSqrt(AbstractOperator first) {
        super(first);
    }

    @Override
    protected String getType() {
        return "sqrt";
    }

    @Override
    protected double internalCalc(double x) {
        throw new UnsupportedOperationException("Doubles are not safe");
    }

    @Override
    protected int internalCalc(int x) {
        if (x < 0) {
            throw new OverflowException();
        }
        return (int) Math.sqrt(x);
    }
}
