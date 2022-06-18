package info.kgeorgiy.ja.yakupov.bank.utils;

import info.kgeorgiy.ja.yakupov.bank.bank.Bank;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * Special test for bank, that run numerous of tests in parallel
 */
public class ParallelTests implements Test {
    private final Collection<Test> tests = new ArrayList<>();

    public void addTest(final Test test) {
        tests.add(test);
    }

    @Override
    public void test(final Bank bank) throws RemoteException {
        final ExecutorService workers = Executors.newFixedThreadPool(8);
        tests.forEach(t -> workers.submit(() -> {
            try {
                t.test(bank);
            } catch (final RemoteException e) {
                throw new UncheckedIOException(e);
            }
        }));

        workers.shutdown();
        while (true) {
            try {
                if (workers.awaitTermination(1L, TimeUnit.MINUTES)) {
                    break;
                }
            } catch (final InterruptedException ignored) {}
        }
    }
}
