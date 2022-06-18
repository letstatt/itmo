package expression.operators;

import expression.AbstractCalculator;

public class Square<T> extends UnaryOperator<T> {

    public Square(AbstractOperator<T> val) {
        super(val);
    }

    @Override
    protected T internalCalc(final T val, AbstractCalculator<T> calc) {
        return calc.multiply(val, val);
    }
}
