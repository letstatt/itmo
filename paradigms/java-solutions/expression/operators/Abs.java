package expression.operators;

import expression.AbstractCalculator;

public class Abs<T> extends UnaryOperator<T> {

    public Abs(AbstractOperator<T> val) {
        super(val);
    }

    @Override
    protected T internalCalc(final T val, AbstractCalculator<T> calc) {
        return (calc.sign(val) ? calc.negate(val) : val);
    }
}
