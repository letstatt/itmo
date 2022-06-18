package info.kgeorgiy.ja.yakupov.bank.account;

import java.io.IOException;
import java.io.Serializable;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Storage for {@code Account} instances. Each person in the bank should have this one.
 */
public final class AccountsHolder implements Serializable {

    private final ConcurrentMap<String, Account> accounts;

    /**
     * Default constructor
     */
    public AccountsHolder() {
        this(new ConcurrentHashMap<>());
    }

    /**
     * Shallow copy constructor
     * @param accounts Accounts map instance
     */
    public AccountsHolder(final ConcurrentMap<String, Account> accounts) {
        this.accounts = accounts;
    }

    /**
     * Deep copy constructor
     * @param holder {@code AccountsHolder} instance
     */
    public AccountsHolder(final AccountsHolder holder) {
        this();
        // the code below won't produce concurrent modification exception, by java docs
        holder.accounts.forEach((key, value) -> {
            try {
                accounts.put(key, new LocalAccount(value));
            } catch (final IOException e) {
                throw new UncheckedIOException(e);
            }
        });
    }

    /**
     * Create account
     * @param passportId Passport number
     * @param accId Account id
     * @param builder {@code AccountBuilder} instance
     * @return {@code Account} instance
     */
    public Account createAccount(final int passportId, final String accId, final AccountBuilder builder) {
        return accounts.computeIfAbsent(getFormattedAccountId(passportId, accId), builder::build);
    }

    /**
     * Lookup for an account
     * @param passportId Passport number
     * @param accId Account id
     * @return {@code Account} instance or {@code null} if it doesn't exist
     */
    public Account getAccount(final int passportId, final String accId) {
        return accounts.getOrDefault(getFormattedAccountId(passportId, accId), null);
    }

    /**
     * Return accounts map
     * @return {@code Map<String, Account>} instance
     */
    public Map<String, Account> getAccounts() {
        return accounts;
    }

    private static String getFormattedAccountId(final int passportId, final String accId) {
        return passportId + ":" + accId;
    }
}
