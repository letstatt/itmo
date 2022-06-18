package info.kgeorgiy.ja.yakupov.bank.person;

import info.kgeorgiy.ja.yakupov.bank.account.Account;
import info.kgeorgiy.ja.yakupov.bank.account.RemoteAccount;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * Remotely-synchronized {@code Person} instance
 */
public final class RemotePerson extends AbstractPerson {
    private final int port;

    /**
     * Create remotely-synchronized {@code Person} instance
     * @param name Name
     * @param surname Surname
     * @param passportId Passport number
     */
    public RemotePerson(final String name, final String surname, final int passportId, final int port) {
        super(name, surname, passportId, RemoteAccount::new);
        this.port = port;
    }

    @Override
    public Account createAccount(final String accId) {
        Account account = getAccount(accId);
        if (account != null) {
            return account;
        }

        try {
            // :NOTE: Двойное исполнение
            account = super.createAccount(accId);
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } catch (final RemoteException e) {
            throw new UncheckedIOException(e);
        }
    }
}
