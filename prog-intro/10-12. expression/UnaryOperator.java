package expression;

import java.util.Objects;

public abstract class UnaryOperator extends AbstractOperator {
    protected final AbstractOperator a;

    public UnaryOperator(AbstractOperator a) {
        this.a = a;
    }

    @Override
    public Priority getPriority() {
        return Priority.HIGHEST;
    }

    protected abstract double internalCalc(double x);
    protected abstract int internalCalc(int x);

    @Override
    public double evaluate(double x) {
        return internalCalc(a.evaluate(x));
    }

    @Override
    public int evaluate(int x) {
        return internalCalc(a.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return internalCalc(a.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "(" + getType() + a + ")";
    }

    @Override
    public String toMiniString() {
        if (a.getPriority() == Priority.HIGHEST) {
            return getType() + a.toMiniString();
        } else {
            return getType() + "(" + a.toMiniString() + ")";
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(a, this.getClass());
    }
}
