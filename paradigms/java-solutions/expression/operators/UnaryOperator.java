package expression.operators;

import expression.AbstractCalculator;

public abstract class UnaryOperator<T> extends AbstractOperator<T> {

    protected final AbstractOperator<T> val;

    protected abstract T internalCalc(final T val, AbstractCalculator<T> calc);

    public UnaryOperator(AbstractOperator<T> val) {
        this.val = val;
    }

    @Override
    public T evaluate(final T x, final T y, final T z, AbstractCalculator<T> calc) {
        return internalCalc(val.evaluate(x, y, z, calc), calc);
    }
}
