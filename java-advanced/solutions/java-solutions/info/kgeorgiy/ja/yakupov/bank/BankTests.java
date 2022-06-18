package info.kgeorgiy.ja.yakupov.bank;

import info.kgeorgiy.ja.yakupov.bank.account.Account;
import info.kgeorgiy.ja.yakupov.bank.account.LocalAccount;
import info.kgeorgiy.ja.yakupov.bank.person.LocalPerson;
import info.kgeorgiy.ja.yakupov.bank.person.Person;
import info.kgeorgiy.ja.yakupov.bank.utils.ParallelTests;
import info.kgeorgiy.ja.yakupov.bank.utils.TestChain;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.JUnitCore;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.util.*;
import java.util.random.RandomGenerator;
import java.util.stream.IntStream;

/**
 * Tests for bank
 */
public class BankTests {

    @Test
    public void test1_remoteAccountsEquality() {
        test(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            final Person samePerson = bank.createPerson("Name", "Surname", 1234);

            Assert.assertTrue(person.equals(samePerson)); // links are same
            Assert.assertEquals("Name", person.getName());
            Assert.assertEquals("Surname", person.getSurname());
            Assert.assertEquals(1234, person.getPassportId());
        });
    }

    private void test(final info.kgeorgiy.ja.yakupov.bank.utils.Test test) {
        new TestChain().addTest(test).run();
    }

    @Test
    public void test2_credentials() {
        test(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            final Person samePerson = bank.createPerson("Evil", "Evil", 1234);

            Assert.assertNull(bank.getRemotePerson(0));
            Assert.assertNull(bank.getLocalPerson(0));

            Assert.assertNull(samePerson);
            // :NOTE: Дубли
            Assert.assertEquals("Name", person.getName());
            Assert.assertEquals("Surname", person.getSurname());
        });
    }

    @Test
    public void test3_oneAccount() {
        new TestChain().addTest(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            final Account account = person.createAccount("myAcc1");

            account.setAmount(100);
            Assert.assertEquals(100, account.getAmount());
            account.incrementAmount(10);
            account.incrementAmount(10);
            Assert.assertEquals(120, account.getAmount());
            Assert.assertEquals(account, // links are same
                    person.getAccount("myAcc1"));
        }).run();
    }

    @Test
    public void test4_twoAccounts() {
        new TestChain().addTest(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            final Account account1 = person.createAccount("myAcc1");
            final Account account2 = person.createAccount("myAcc2");

            account1.setAmount(1337);
            account2.setAmount(1338);
            Assert.assertEquals(1337, account1.getAmount());
            Assert.assertEquals(1338, account2.getAmount());
        }).run();
    }

    @Test
    public void test5_lotsOfAccounts() {
        new TestChain().addTest(bank -> {
            final RandomGenerator random = new Random(1);
            final int accountsCount = 10;
            int total = 0;

            final Person person = bank.createPerson("Name", "Surname", 1234);

            for (int i = 0; i < accountsCount; ++i) {
                final Account account = person.createAccount("acc" + i);
                final int delta = random.nextInt();
                account.incrementAmount(i * delta);
                total += i * delta;
            }

            final Map<String, Account> accounts = person.getAccounts();
            Assert.assertEquals(accountsCount, accounts.size());
            int got = 0;

            for (final Account acc : accounts.values()) {
                got += acc.getAmount();
            }

            Assert.assertEquals(total, got);
        }).run();
    }

    @Test
    public void test6_localPersons() {
        new TestChain().addTest(bank -> {
            final Account account = bank
                    .createPerson("Name", "Surname", 1234)
                    .createAccount("acc1");

            account.setAmount(100);
            Assert.assertEquals(100, account.getAmount());

            final Person remote = bank.getRemotePerson(1234);
            final LocalPerson local = bank.getLocalPerson(1234);

            Assert.assertNotEquals(
                    remote.getAccount("acc1"),
                    local.getAccount("acc1"));

            account.setAmount(200);
            Assert.assertEquals(100, local.getAccount("acc1").getAmount());
            Assert.assertEquals(200, remote.getAccount("acc1").getAmount());

            local.createAccount("acc2").setAmount(1337);
            remote.createAccount("acc2").setAmount(1338);

            local.getAccount("acc2").incrementAmount(10);
            remote.getAccount("acc2").incrementAmount(10);

            Assert.assertEquals(1347, local.getAccount("acc2").getAmount());
            Assert.assertEquals(1348, remote.getAccount("acc2").getAmount());

            remote.createAccount("acc3");
            Assert.assertNotNull(remote.getAccount("acc3"));
            Assert.assertNull(local.getAccount("acc3"));
        }).run();
    }

    @Test
    public void test7_multithreading() {
        final RandomGenerator random = new Random(1);

        final TestChain chain = new TestChain().addTest(
                bank -> bank.createPerson("Name", "Surname", 1234).createAccount("acc"));

        final ParallelTests test = new ParallelTests();
        final int count = random.nextInt(1000, 2000);
        final int[] total = {0}; // :NOTE: ??

        IntStream.range(0, count)
                .map(i -> random.nextInt(-100, 100))
                .peek(delta -> total[0] += delta)
                .forEach(delta -> test.addTest(bank -> bank
                        .getRemotePerson(1234)
                        .getAccount("acc")
                        .incrementAmount(delta)
                ));

        final int[] got = {0};

        chain.addTest(test)
                .addTest(bank -> got[0] = bank
                        .getRemotePerson(1234)
                        .getAccount("acc").getAmount())
                .run();

        Assert.assertEquals(total[0], got[0]);
    }

    @Test
    public void test8_concurrentCopy() {
        final RandomGenerator random = new Random(2);

        final TestChain chain = new TestChain().addTest(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            person.createAccount("acc1");
            person.createAccount("acc2");
            person.createAccount("acc3");
        });

        final ParallelTests test = new ParallelTests();
        final int count = random.nextInt(1000, 5000);
        final int[] total = {0};

        IntStream.range(0, count).forEach(i -> {
            final int delta1 = random.nextInt(-100, 100);
            final int delta2 = random.nextInt(-100, 100);
            final int delta3 = random.nextInt(-100, 100);
            total[0] += delta1 + delta2 + delta3;
            test.addTest(bank -> {
                final Person person = bank.getRemotePerson(1234);
                person.getAccount("acc1").incrementAmount(delta1);
                person.getAccount("acc2").incrementAmount(delta2);
                person.getAccount("acc3").incrementAmount(delta3);
                bank.getLocalPerson(1234); // it shouldn't raise ConcurrentModificationException
            });
        });

        final int[] got = {0};

        chain.addTest(test).addTest(bank -> {
            final Person person = bank.getLocalPerson(1234);
            got[0] += person.getAccount("acc1").getAmount();
            got[0] += person.getAccount("acc2").getAmount();
            got[0] += person.getAccount("acc3").getAmount();
        }).run();

        Assert.assertEquals(total[0], got[0]);
    }

    @Test
    public void test9_accountOverflow() {
        new TestChain().addTest(bank -> {
            final Account account = bank.createPerson("Name", "Surname", 1234).createAccount("acc");

            account.setAmount(Integer.MAX_VALUE);
            account.incrementAmount(-1);

            try {
                account.incrementAmount(+2);
                Assert.fail("ArithmeticException expected");
            } catch (final ArithmeticException ignored) {}

            account.setAmount(Integer.MIN_VALUE);
            account.incrementAmount(+2);

            try {
                account.incrementAmount(-3);
                Assert.fail("ArithmeticException expected");
            } catch (final ArithmeticException ignored) {}
        }).run();
    }

    @Test
    public void test10_directAccountsMapModification() {
        new TestChain().addTest(bank -> {
            final Person person = bank.createPerson("Name", "Surname", 1234);
            person.createAccount("acc1");
            person.createAccount("acc2");

            try {
                final Account account = new LocalAccount("acc3");
                person.getAccounts().put("hac", account);
                Assert.fail("UnsupportedOperationException expected");
            } catch (final UnsupportedOperationException ignored) {}

            try {
                person.getAccounts().remove("acc1");
                Assert.fail("UnsupportedOperationException expected");
            } catch (final UnsupportedOperationException ignored) {}
        }).run();
    }

    @Test
    public void test11_i18n() {
        new TestChain().addTest(bank -> {
            final String[] names = {
                    "Name Surname",
                    "Andrew Stankevich",
                    "Ильгиз Якупов",
                    "艾古 利",
                    "Gēnn àjì",
                    "サーシ ャ",
                    "ﺔ ﺐ",
                    "\uD800\uDF0D \uD800\uDF0F"
            };

            final RandomGenerator random = new Random(3);
            final List<Integer> ids = new ArrayList<>();

            for (final String s: names) {
                final String[] name = s.split(" ");
                ids.add(random.nextInt());
                bank.createPerson(name[0], name[1], ids.get(ids.size() - 1));
            }

            for (final int id: ids) {
                final LocalPerson person = bank.getLocalPerson(id);
                Assert.assertNotNull(person);
                final String name = person.getName() + " " + person.getSurname();
                Assert.assertTrue(
                        Arrays.asList(names).contains(name));
            }
        }).run();
    }

    /**
     * Init method for tests
     */
    @BeforeClass
    public static void init() {
        try {
            LocateRegistry.createRegistry(TestChain.REGISTRY_PORT);

        } catch (final RemoteException e) {
            throw new IllegalStateException("Registry could not be exported: " + e.getMessage());
        }
    }

    /**
     * Start CLI application
     * @param args no arguments actually needed
     */
    public static void main(final String[] args) {
        final JUnitCore jUnitCore = new JUnitCore();
        jUnitCore.run(BankTests.class);
        System.exit(0);
    }
}
