import java.util.Arrays;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank implementation.
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    @Override
    public long getAmount(int index) {
        accounts[index].lock.lock();
        final long result = accounts[index].amount;
        accounts[index].lock.unlock();
        return result;
    }

    @Override
    public long getTotalAmount() {
        long sum = 0;
        Arrays.stream(accounts).forEach(a -> a.lock.lock());
        for (final Account account : accounts) {
            sum += account.amount;
        }
        Arrays.stream(accounts).forEach(a -> a.lock.unlock());
        return sum;
    }

    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        final Account account = accounts[index];
        account.lock.lock();
        if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT) {
            account.lock.unlock();
            throw new IllegalStateException("Overflow");
        }
        account.amount += amount;
        final long result = account.amount;
        account.lock.unlock();
        return result;
    }

    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        final Account account = accounts[index];
        account.lock.lock();
        if (account.amount - amount < 0) {
            account.lock.unlock();
            throw new IllegalStateException("Underflow");
        }
        account.amount -= amount;
        final long result = account.amount;
        account.lock.unlock();
        return result;
    }

    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        final Account from = accounts[fromIndex];
        final Account to = accounts[toIndex];

        final ReentrantLock lock1 = accounts[Math.min(fromIndex, toIndex)].lock;
        final ReentrantLock lock2 = accounts[Math.max(fromIndex, toIndex)].lock;

        // should be locked together
        lock1.lock();
        lock2.lock();
        if (amount > from.amount) {
            lock2.unlock();
            lock1.unlock();
            throw new IllegalStateException("Underflow");
        } else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT) {
            lock2.unlock();
            lock1.unlock();
            throw new IllegalStateException("Overflow");
        }
        from.amount -= amount;
        to.amount += amount;
        lock2.unlock();
        lock1.unlock();
    }

    /**
     * Private account data structure.
     */
    static class Account {
        /**
         * Amount of funds in this account.
         */
        long amount;
        ReentrantLock lock = new ReentrantLock();
    }
}
