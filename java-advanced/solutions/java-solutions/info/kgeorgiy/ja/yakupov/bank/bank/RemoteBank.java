package info.kgeorgiy.ja.yakupov.bank.bank;

import info.kgeorgiy.ja.yakupov.bank.person.LocalPerson;
import info.kgeorgiy.ja.yakupov.bank.person.Person;
import info.kgeorgiy.ja.yakupov.bank.person.RemotePerson;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Basic implementation of {@code Bank} interface
 */
public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<Integer, RemotePerson> persons = new ConcurrentHashMap<>();

    /**
     * Default constructor
     * @param port Port to RMI Registry
     */
    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public LocalPerson getLocalPerson(final int passportId) throws RemoteException {
        final RemotePerson person = persons.getOrDefault(passportId, null);
        return person == null ? null : new LocalPerson(person);
    }

    @Override
    public RemotePerson getRemotePerson(final int passportId) {
        return persons.get(passportId);
    }

    @Override
    public synchronized Person createPerson(final String name, final String surname, final int passportId) throws RemoteException {
        // :NOTE: cast
        RemotePerson person = getRemotePerson(passportId);

        if (person == null) {
            person = new RemotePerson(name, surname, passportId, port);
            // :NOTE: Сеть
            UnicastRemoteObject.exportObject(person, port); // make person public
            persons.put(passportId, person);
            return person;
        } else if (person.getName().equals(name) && person.getSurname().equals(surname)) {
            return person;
        } else {
            return null;
        }
    }
}
