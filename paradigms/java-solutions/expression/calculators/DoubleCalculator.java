package expression.calculators;

import expression.AbstractCalculator;

public class DoubleCalculator extends AbstractCalculator<Double> {

    @Override
    public Double negate(final Double x) {
        return -x;
    }

    @Override
    public boolean sign(final Double x) {
        return Double.compare(x, 0.0) < 0;
    }

    @Override
    public Double add(final Double x, final Double y) {
        return x + y;
    }

    @Override
    public Double subtract(final Double x, final Double y) {
        return x - y;
    }

    @Override
    public Double multiply(final Double x, final Double y) {
        return x * y;
    }

    @Override
    public Double div(final Double x, final Double y) {
        return x / y;
    }

    @Override
    public Double mod(final Double x, final Double y) {
        return x % y;
    }

    @Override
    public Double parse(String x) {
        return Double.valueOf(x);
    }
}