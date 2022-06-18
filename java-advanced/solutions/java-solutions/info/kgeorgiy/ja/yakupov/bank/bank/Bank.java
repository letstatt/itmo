package info.kgeorgiy.ja.yakupov.bank.bank;

import info.kgeorgiy.ja.yakupov.bank.person.LocalPerson;
import info.kgeorgiy.ja.yakupov.bank.person.Person;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Returns @{code LocalPerson} registered by the specified passport number
     *
     * @param passportId passport number
     * @return LocalPerson class or {@code null} if such account does not exist.
     */
    LocalPerson getLocalPerson(final int passportId) throws RemoteException;

    /**
     * Returns @{code RemotePerson} registered by the specified passport number
     *
     * @param passportId passport number
     * @return RemotePerson instance (hidden by Person interface) or {@code null} if such account does not exist.
     */
    Person getRemotePerson(final int passportId) throws RemoteException;

    /**
     * Creates new personality if absent
     * @param name Name
     * @param surname Surname
     * @param passportId Passport number
     * @return {@code Person} instance, including all given information, or {@code null} if the person
     * by passportId exists, but credentials don't match
     */
    Person createPerson(final String name, final String surname, final int passportId) throws RemoteException;

    //Account createAccount(final Account account, final )
}
