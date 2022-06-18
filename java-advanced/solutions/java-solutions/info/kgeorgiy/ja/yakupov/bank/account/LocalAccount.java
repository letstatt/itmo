package info.kgeorgiy.ja.yakupov.bank.account;

import java.rmi.RemoteException;

/**
 * Non-remotely-synchronized (local) {@code Account} instance
 */
public final class LocalAccount extends AbstractAccount {

    /**
     * Create non-remote {@code Account} instance
     * @param id Account id
     */
    public LocalAccount(final String id) {
        super(id);
    }

    /**
     * Create non-remote {@code Account} instance from remote one
     * @param account {@code RemoteAccount} instance to copy
     */
    public LocalAccount(final Account account) throws RemoteException {
        this(account.getId());
        setAmount(account.getAmount());
    }
}
