package expression.calculators;

import expression.AbstractCalculator;
import expression.exceptions.DivisionByZeroException;

public class ShortCalculator extends AbstractCalculator<Short> {

    private Short castToShort(final int x) {
        return parse(Integer.toString(x));
    }

    @Override
    public Short negate(final Short x) {
        return castToShort(x * -1);
    }

    @Override
    public boolean sign(final Short x) {
        return x < 0;
    }

    @Override
    public Short add(final Short x, final Short y) {
        return castToShort(x + y);
    }

    @Override
    public Short subtract(final Short x, final Short y) {
        return castToShort(x - y);
    }

    @Override
    public Short multiply(final Short x, final Short y) {
        return castToShort(x * y);
    }

    @Override
    public Short div(final Short x, final Short y) {
        if (y == 0) {
            throw new DivisionByZeroException();
        }

        return castToShort(x / y);
    }

    @Override
    public Short mod(final Short x, final Short y) {
        if (y == 0) {
            throw new DivisionByZeroException();
        }

        return castToShort(x % y);
    }

    @Override
    public Short parse(String x) {
        return (short) Integer.parseInt(x);
    }
}
