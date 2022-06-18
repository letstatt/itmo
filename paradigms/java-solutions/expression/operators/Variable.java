package expression.operators;

import expression.AbstractCalculator;
import expression.exceptions.VariableLookupException;

public class Variable<T> extends AbstractOperator<T> {

    private final String variable;

    public Variable(String variable) {
        this.variable = variable;
    }

    @Override
    public T evaluate(final T x, final T y, final T z, AbstractCalculator<T> calc) {
        switch (variable) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new VariableLookupException(
                        "Undefined variable: " + variable
                );
        }
    }
}
