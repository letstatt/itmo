package expression.operators;

import expression.AbstractCalculator;

public class Negate<T> extends UnaryOperator<T> {

    public Negate(AbstractOperator<T> val) {
        super(val);
    }

    @Override
    protected T internalCalc(final T val, AbstractCalculator<T> calc) {
        return calc.negate(val);
    }
}
