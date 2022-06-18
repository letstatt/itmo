package expression;

import java.util.Objects;

public class Variable extends AbstractOperator {
    private final String name;

    public Variable(final String name) {
        this.name = name;
    }

    @Override
    public Priority getPriority() {
        return Priority.HIGHEST;
    }

    @Override
    public double evaluate(double x) {
        return x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        switch (name) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new IllegalArgumentException("Cannot set a variable " + name);
        }
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String toMiniString() {
        return name;
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
