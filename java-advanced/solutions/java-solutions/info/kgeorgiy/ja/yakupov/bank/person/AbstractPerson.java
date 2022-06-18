package info.kgeorgiy.ja.yakupov.bank.person;

import info.kgeorgiy.ja.yakupov.bank.account.Account;
import info.kgeorgiy.ja.yakupov.bank.account.AccountBuilder;
import info.kgeorgiy.ja.yakupov.bank.account.AccountsHolder;

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;

/**
 * Basic implementation of {@code Person} interface
 */
public abstract class AbstractPerson implements Person, Serializable {
    protected final Credentials credentials;
    protected final AccountsHolder accounts;
    protected final AccountBuilder accountBuilder;

    /**
     * Basic constructor
     * @param credentials {@code Credentials} instance
     * @param accounts {@code AccountHolder} instance
     * @param accountBuilder {@code AccountBuilder} instance
     */
    public AbstractPerson(final Credentials credentials, final AccountsHolder accounts,
                          final AccountBuilder accountBuilder) {
        this.credentials = credentials;
        this.accounts = accounts;
        this.accountBuilder = accountBuilder;
    }

    /**
     * Constructor without {@code Credentials} instance
     * @param name Name
     * @param surname Surname
     * @param passportId Passport number
     * @param builder {@code AccountBuilder} instance
     */
    public AbstractPerson(final String name, final String surname, final int passportId,
                          final AccountBuilder builder) {
        this(new Credentials(name, surname, passportId), new AccountsHolder(), builder);
    }

    /**
     * Constructor without {@code Credentials} instance, copying of {@code AccountsHolder}
     * @param name Name
     * @param surname Surname
     * @param passportId Passport number
     * @param accounts {@code AccountsHolder} instance
     * @param builder {@code AccountBuilder} instance
     */
    public AbstractPerson(final String name, final String surname, final int passportId,
                          final AccountsHolder accounts, final AccountBuilder builder) {
        this(new Credentials(name, surname, passportId), accounts, builder);
    }

    @Override
    public String getName() {
        return credentials.name();
    }

    @Override
    public String getSurname() {
        return credentials.surname();
    }

    @Override
    public int getPassportId() {
        return credentials.passportId();
    }

    @Override
    public Map<String, Account> getAccounts() {
        return Collections.unmodifiableMap(accounts.getAccounts());
    }

    @Override
    public Account createAccount(final String accId) {
        return accounts.createAccount(getPassportId(), accId, accountBuilder);
    }

    @Override
    public Account getAccount(final String accId) {
        return accounts.getAccount(getPassportId(), accId);
    }

}
