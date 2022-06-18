package info.kgeorgiy.ja.yakupov.bank.account;

import java.io.Serializable;

/**
 * Functional interface providing {@code Account} instances construction
 */
@FunctionalInterface
public interface AccountBuilder extends Serializable {
    /**
     * Create {@code Account} instance
     * @param id Account id
     * @return created {@code Account} instance
     */
    Account build(final String id);
}
