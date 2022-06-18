package expression.calculators;

import expression.AbstractCalculator;
import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public class LongCalculator extends AbstractCalculator<Long> {

    @Override
    public Long negate(final Long x) {
        return -x;
    }

    @Override
    public boolean sign(final Long x) {
        return x < 0;
    }

    @Override
    public Long add(final Long x, final Long y) {
        return x + y;
    }

    @Override
    public Long subtract(final Long x, final Long y) {
        return x - y;
    }

    @Override
    public Long multiply(final Long x, final Long y) {
        return x * y;
    }

    @Override
    public Long div(final Long x, final Long y) {
        if (y == 0) {
            throw new DivisionByZeroException();
        }
        return x / y;
    }

    @Override
    public Long mod(final Long x, final Long y) {
        if (y == 0) {
            throw new DivisionByZeroException();
        }
        return x % y;
    }

    @Override
    public Long parse(String x) {
        return Long.valueOf(x);
    }
}
