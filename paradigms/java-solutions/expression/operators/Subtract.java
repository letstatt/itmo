package expression.operators;

import expression.AbstractCalculator;

public class Subtract<T> extends BinaryOperator<T> {

    public Subtract(AbstractOperator<T> a, AbstractOperator<T> b) {
        super(a, b);
    }

    @Override
    protected T internalCalc(final T x, final T y, AbstractCalculator<T> calc) {
        return calc.subtract(x, y);
    }
}

