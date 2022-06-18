package expression.operators;

import expression.AbstractCalculator;

public class Multiply<T> extends BinaryOperator<T> {

    public Multiply(AbstractOperator<T> a, AbstractOperator<T> b) {
        super(a, b);
    }

    @Override
    protected T internalCalc(final T x, final T y, AbstractCalculator<T> calc) {
        return calc.multiply(x, y);
    }
}

