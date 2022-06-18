package expression.calculators;

import expression.AbstractCalculator;
import expression.exceptions.DivisionByZeroException;

import java.math.BigInteger;

public class BigIntegerCalculator extends AbstractCalculator<BigInteger> {

    @Override
    public BigInteger negate(final BigInteger x) {
        return x.negate();
    }

    @Override
    public boolean sign(final BigInteger x) {
        return x.signum() < 0;
    }

    @Override
    public BigInteger add(final BigInteger x, final BigInteger y) {
        return x.add(y);
    }

    @Override
    public BigInteger subtract(final BigInteger x, final BigInteger y) {
        return x.subtract(y);
    }

    @Override
    public BigInteger multiply(final BigInteger x, final BigInteger y) {
        return x.multiply(y);
    }

    @Override
    public BigInteger div(final BigInteger x, final BigInteger y) {
        if (BigInteger.ZERO.equals(y)) {
            throw new DivisionByZeroException();
        }

        return x.divide(y);
    }

    @Override
    public BigInteger mod(final BigInteger x, final BigInteger y) {
        if (BigInteger.ZERO.compareTo(y) >= 0) {
            throw new DivisionByZeroException();
        }
        return x.mod(y);
    }

    @Override
    public BigInteger parse(String x) {
        return new BigInteger(x);
    }
}
