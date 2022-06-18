package info.kgeorgiy.ja.yakupov.bank.utils;

import info.kgeorgiy.ja.yakupov.bank.bank.Bank;
import info.kgeorgiy.ja.yakupov.bank.bank.RemoteBank;

import java.rmi.AccessException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Test chain for bank
 */
public class TestChain {
    public static final int REGISTRY_PORT = 44501;
    private final Collection<Test> tests = new ArrayList<>();

    /**
     * Default constructor. Do nothing.
     */
    public TestChain() {}

    /**
     * Add test to chain
     * @param test test to queue
     * @return This chain
     */
    public TestChain addTest(final Test test) {
        tests.add(test);
        return this;
    }

    /**
     * Execute all tests in chain one by one
     */
    public void run() {
        try {
            final Remote bank = new RemoteBank(REGISTRY_PORT);
            final Bank stub = (Bank) UnicastRemoteObject.exportObject(bank, REGISTRY_PORT);
            final Registry registry = LocateRegistry.getRegistry(REGISTRY_PORT);
            registry.rebind("bank", bank);

            for (final Test t: tests) {
                t.test(stub);
            }

            UnicastRemoteObject.unexportObject(bank, false);
            registry.unbind("bank");

        } catch (final AccessException e) {
            throw new RuntimeException("Registry access denied", e);

        } catch (final RemoteException e) {
            throw new RuntimeException("Unable to access registry", e);

        } catch (final NotBoundException e) {
            throw new RuntimeException("Bank is not bound", e);
        }
    }
}
