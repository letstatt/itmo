package expression;

import java.util.Map;

public enum BinaryOperatorsMap {
    ADD, SUBTRACT, MULTIPLY, DIVIDE, BITWISE_OR, BITWISE_XOR, BITWISE_AND, MIN, MAX;

    public static final Map<String, BinaryOperatorsMap> hints = Map.of(
            "+", ADD,
            "-", SUBTRACT,
            "*", MULTIPLY,
            "/", DIVIDE,
            "|", BITWISE_OR,
            "^", BITWISE_XOR,
            "&", BITWISE_AND,
            "min", MIN,
            "max", MAX
    );

    public static final Map<BinaryOperatorsMap, Priority> priors = Map.of(
            MIN, Priority.LOWEST,
            MAX, Priority.LOWEST,
            BITWISE_OR, Priority.BITWISE_OR,
            BITWISE_XOR,Priority.BITWISE_XOR,
            BITWISE_AND,Priority.BITWISE_AND,
            ADD, Priority.ADDITION,
            SUBTRACT, Priority.ADDITION,
            MULTIPLY, Priority.MULTIPLY,
            DIVIDE, Priority.MULTIPLY
    );
}
