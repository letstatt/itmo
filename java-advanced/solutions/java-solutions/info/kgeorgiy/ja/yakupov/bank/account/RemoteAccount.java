package info.kgeorgiy.ja.yakupov.bank.account;

/**
 * Remotely-synchronized {@code Account} instance
 */
public final class RemoteAccount extends AbstractAccount {

    /**
     * Create remotely-synchronized {@code Account} instance
     * @param id Account id
     */
    public RemoteAccount(final String id) {
        super(id);
    }
}
