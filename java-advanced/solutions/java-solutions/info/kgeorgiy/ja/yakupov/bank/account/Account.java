package info.kgeorgiy.ja.yakupov.bank.account;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Account extends Remote {
    /** Returns account identifier. */
    String getId() throws RemoteException;

    /** Returns amount of money at the account. */
    int getAmount() throws RemoteException;

    /** Sets amount of money at the account. */
    void setAmount(int amount) throws RemoteException;

    /** Adds amount of money at the account.
     * @param amount Amount of money to be added
     * */
    void incrementAmount(int amount) throws RemoteException;
}