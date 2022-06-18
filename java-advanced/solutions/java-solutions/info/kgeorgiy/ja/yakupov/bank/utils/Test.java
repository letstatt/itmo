package info.kgeorgiy.ja.yakupov.bank.utils;

import info.kgeorgiy.ja.yakupov.bank.bank.Bank;

import java.rmi.RemoteException;

/**
 * Test interface for tests for bank
 */
public interface Test {
    void test(Bank bank) throws RemoteException;
}
