package expression.operators;

import expression.AbstractCalculator;

public class Divide<T> extends BinaryOperator<T> {

    public Divide(AbstractOperator<T> a, AbstractOperator<T> b) {
        super(a, b);
    }

    @Override
    protected T internalCalc(final T x, final T y, AbstractCalculator<T> calc) {
        return calc.div(x, y);
    }
}

