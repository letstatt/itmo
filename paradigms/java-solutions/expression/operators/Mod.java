package expression.operators;

import expression.AbstractCalculator;

public class Mod<T> extends BinaryOperator<T> {

    public Mod(AbstractOperator<T> a, AbstractOperator<T> b) {
        super(a, b);
    }

    @Override
    protected T internalCalc(final T x, final T y, AbstractCalculator<T> calc) {
        return calc.mod(x, y);
    }
}

