package info.kgeorgiy.ja.yakupov.bank.person;

import info.kgeorgiy.ja.yakupov.bank.account.Account;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

public interface Person extends Remote {

    /**
     * Gets first name of person
     * @return first name
     */
    String getName() throws RemoteException;

    /**
     * Gets second name of person
     * @return second name
     */
    String getSurname() throws RemoteException;

    /**
     * Gets passport number of person
     * @return passport id
     */
    int getPassportId() throws RemoteException;

    /**
     * Gives an unmodifiable view to all person's accounts
     * @return {@code Map<String, Account>} instance
     */
    Map<String, Account> getAccounts() throws RemoteException;

    /**
     * Returns {@code Account} instance
     * @param accId account id
     * @return {@code Account} instance or {@code null} if it doesn't exist
     */
    Account getAccount(String accId) throws RemoteException;

    /**
     * Adds {@code Account} to this Person
     * @param accId account id
     */
    Account createAccount(String accId) throws RemoteException;
}
