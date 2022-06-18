package expression;

import java.util.Objects;

public abstract class BinaryOperator extends AbstractOperator {
    protected final AbstractOperator a, b;

    public BinaryOperator(AbstractOperator a, AbstractOperator b) {
        this.a = a;
        this.b = b;
    }

    protected abstract double internalCalc(double x, double y);
    protected abstract int internalCalc(int x, int y);

    @Override
    public double evaluate(double x) {
        return internalCalc(a.evaluate(x), b.evaluate(x));
    }

    @Override
    public int evaluate(int x) {
        return internalCalc(a.evaluate(x), b.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return internalCalc(a.evaluate(x, y, z), b.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "(" + a + " " + getType() + " " + b + ")";
    }

    private String bracketsAddition(AbstractOperator e, boolean brackets) {
        if (brackets) {
            return "(" + e.toMiniString() + ")";
        } else {
            return e.toMiniString();
        }
    }

    @Override
    public String toMiniString() {
        return bracketsAddition(a, a.compareTo(this) < 0) +
                " " + getType() + " " +
                bracketsAddition(b, b.compareTo(this) < 0
                        || b.compareTo(this) == 0 && (!b.unwrappable() || !this.unwrappable())
                );
    }

    @Override
    public int hashCode() {
        return Objects.hash(a, b, this.getClass());
    }
}
