package expression.operators;

import expression.AbstractCalculator;

public class Add<T> extends BinaryOperator<T> {

    public Add(AbstractOperator<T> a, AbstractOperator<T> b) {
        super(a, b);
    }

    @Override
    protected T internalCalc(final T x, final T y, AbstractCalculator<T> calc) {
        return calc.add(x, y);
    }
}

