package info.kgeorgiy.ja.yakupov.bank;

import info.kgeorgiy.ja.yakupov.bank.account.Account;
import info.kgeorgiy.ja.yakupov.bank.bank.Bank;
import info.kgeorgiy.ja.yakupov.bank.bank.RemoteBank;
import info.kgeorgiy.ja.yakupov.bank.person.Person;

import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Arrays;
import java.util.Objects;

public class ConsoleInteractor {
    private static final int REGISTRY_PORT = 44501;

    /**
     * Start console client simulator
     * @param args String array<br/>
     *             args[0] - Name
     *             args[1] - Surname
     *             args[2] - Passport Id
     *             args[3] - Account Id
     *             args[4] - How much to add
     */
    public static void main(final String[] args) {
        if (args == null || args.length != 5 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println("Usage: ConsoleInteractor Name Surname PassportId AccountId Delta");
            return;
        }

        final int passportId;
        final int delta;

        try {
            passportId = Integer.parseInt(args[2]);
            delta = Integer.parseInt(args[4]);
        } catch (final NumberFormatException e) {
            System.out.println("Incorrect format");
            return;
        }

        final Registry registry;

        try {
            Bank bank = new RemoteBank(REGISTRY_PORT);
            registry = LocateRegistry.getRegistry("localhost", REGISTRY_PORT);
            try {
                registry.bind("bank",
                        UnicastRemoteObject.exportObject(bank, 0));
            } catch (final AlreadyBoundException ignored) {
                UnicastRemoteObject.unexportObject(bank, false);
            }

            bank = (Bank) registry.lookup("bank");

            System.out.println("[*] Bank instance obtained");

            Person person = bank.getRemotePerson(passportId);

            if (person == null) {
                person = bank.createPerson(args[0], args[1], passportId);
                System.out.println("[*] Person instance created");

            } else {
                System.out.println("[*] Person instance obtained");
            }

            final Account account = person.createAccount(args[3]);

            System.out.println("[*] Amount on account before: " + account.getAmount());
            account.incrementAmount(delta);

            System.out.println("[*] Amount on account after: " + account.getAmount());

        } catch (final RemoteException e) {
            System.err.println("Interaction failed: " + e.getMessage());

        } catch (final NotBoundException e) {
            System.err.println("Bank not found: " + e.getMessage());
        }
    }
}
