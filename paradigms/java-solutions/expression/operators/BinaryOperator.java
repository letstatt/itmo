package expression.operators;

import expression.AbstractCalculator;

public abstract class BinaryOperator<T> extends AbstractOperator<T> {

    protected final AbstractOperator<T> l, r;

    protected abstract T internalCalc(final T x, final T y, AbstractCalculator<T> calc);

    public BinaryOperator(AbstractOperator<T> l, AbstractOperator<T> r) {
        this.l = l;
        this.r = r;
    }

    @Override
    public T evaluate(final T x, final T y, final T z, AbstractCalculator<T> calc) {
        return internalCalc(l.evaluate(x, y, z, calc), r.evaluate(x, y, z, calc), calc);
    }
}
