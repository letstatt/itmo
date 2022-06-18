package expression.operators;

@FunctionalInterface
public interface BinaryOperatorFactory<T> {
    AbstractOperator<T> create(final AbstractOperator<?> a, final AbstractOperator<?> b);
}
