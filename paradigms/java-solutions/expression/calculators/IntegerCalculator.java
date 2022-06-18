package expression.calculators;

import expression.AbstractCalculator;
import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public class IntegerCalculator extends AbstractCalculator<Integer> {

    private final boolean checked;

    public IntegerCalculator(boolean checked) {
        this.checked = checked;
    }

    @Override
    public Integer negate(final Integer x) {
        if (checked && x == Integer.MIN_VALUE) {
            throw new OverflowException(
                    "Negate of " + Integer.MIN_VALUE + " leads to integer overflow"
            );
        }
        return -x;
    }

    @Override
    public boolean sign(final Integer x) {
        return x < 0;
    }

    @Override
    public Integer add(final Integer x, final Integer y) {
        if (checked && ((y > 0 && x > (Integer.MAX_VALUE - y)) ||
                (y < 0 && x < (Integer.MIN_VALUE - y)))) {
            throw new OverflowException(
                    String.format("Sum of %d and %d leads to integer overflow", x, y)
            );
        }
        return x + y;
    }

    @Override
    public Integer subtract(final Integer x, final Integer y) {
        if (checked && ((y > 0 && x < Integer.MIN_VALUE + y) ||
                (y < 0 && x > Integer.MAX_VALUE + y))) {
            throw new OverflowException(
                    String.format("Subtract of %d and %d leads to integer overflow", x, y)
            );
        }
        return x - y;
    }

    @Override
    public Integer multiply(final Integer x, final Integer y) {
        if (checked && !isNotOverMul(x, y)) {
            throw new OverflowException(
                    String.format("Multiply of %d and %d leads to integer overflow", x, y)
            );
        }
        return x * y;
    }

    private boolean isNotOverMul(Integer x, Integer y) {
        if (x > 0) {
            if (y > 0) {
                return x <= (Integer.MAX_VALUE / y);
            }
            return y >= (Integer.MIN_VALUE / x);
        }
        if (y > 0) {
            return x >= (Integer.MIN_VALUE / y);
        }
        return (x == 0) || (y >= (Integer.MAX_VALUE / x));
    }

    @Override
    public Integer div(final Integer x, final Integer y) {
        if (y == 0) {
            throw new DivisionByZeroException();

        } else if (checked && y == -1 && x == Integer.MIN_VALUE) {
            throw new OverflowException(
                    String.format("Division of %d and %d leads to integer overflow", x, y)
            );
        }
        return x / y;
    }

    @Override
    public Integer mod(final Integer x, final Integer y) {
        if (y == 0) {
            throw new DivisionByZeroException();
        }
        return x % y;
    }

    @Override
    public Integer parse(String x) {
        return Integer.valueOf(x);
    }
}
