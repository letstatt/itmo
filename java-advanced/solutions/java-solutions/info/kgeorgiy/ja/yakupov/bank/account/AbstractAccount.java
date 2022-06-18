package info.kgeorgiy.ja.yakupov.bank.account;

import java.io.Serializable;
import java.util.Objects;

/**
 * Basic implementation of {@code Account} interface
 */
public abstract class AbstractAccount implements Serializable, Account {
    private final String id;
    private int amount;

    /**
     * Default constructor
     * @param id Account id
     */
    public AbstractAccount(final String id) {
        this.id = Objects.requireNonNull(id);
        amount = 0;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        return amount;
    }

    @Override
    public synchronized void setAmount(final int amount) {
        this.amount = amount;
    }

    @Override
    public synchronized void incrementAmount(final int amount) {
        long tmp = this.amount;
        tmp += amount;
        if (tmp < Integer.MIN_VALUE) {
            throw new ArithmeticException("Money on account can't be lower than " + Integer.MIN_VALUE);
        } else if (tmp > Integer.MAX_VALUE) {
            throw new ArithmeticException("Money on account can't be more than " + Integer.MAX_VALUE);
        }
        this.amount += amount;
    }
}
