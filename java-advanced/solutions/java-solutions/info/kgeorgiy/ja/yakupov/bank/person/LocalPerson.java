package info.kgeorgiy.ja.yakupov.bank.person;

import info.kgeorgiy.ja.yakupov.bank.account.AccountsHolder;
import info.kgeorgiy.ja.yakupov.bank.account.LocalAccount;

/**
 * Non-remotely-synchronized (local) {@code Person} instance
 */
public final class LocalPerson extends AbstractPerson {
    /**
     * Create non-remote {@code Person} instance
     * @param name Name
     * @param surname Surname
     * @param passportId Passport number
     * @param holder {@code AccountHolder} instance
     */
    public LocalPerson(final String name, final String surname, final int passportId, final AccountsHolder holder) {
        super(name, surname, passportId, holder, LocalAccount::new);
    }

    /**
     * Create non-remote {@code Person} instance from {@code RemotePerson} instance using deep copy
     * @param person {@code RemotePerson} instance
     */
    public LocalPerson(final RemotePerson person) {
        this(
                person.getName(),
                person.getSurname(),
                person.getPassportId(),
                new AccountsHolder(person.accounts)
        );
    }
}
