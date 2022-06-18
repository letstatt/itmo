package expression.operators;

import expression.AbstractCalculator;

public class Const<T> extends AbstractOperator<T> {

    private final T value;

    public Const(final T init) {
        value = init;
    }

    @Override
    public T evaluate(T x, T y, T z, AbstractCalculator<T> calc) {
        return value;
    }
}
