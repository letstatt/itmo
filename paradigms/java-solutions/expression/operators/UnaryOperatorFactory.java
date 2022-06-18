package expression.operators;

@FunctionalInterface
public interface UnaryOperatorFactory<T> {
    AbstractOperator<T> create(final AbstractOperator<?> a);
}
