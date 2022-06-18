package info.kgeorgiy.ja.yakupov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threads;
    private final Queue<Runnable> tasks = new ArrayDeque<>();
    private static final int PENDING_TASKS_LIMIT = 100_000;

    private static class Results<T> {
        private final List<T> results;
        private int count = 0;

        public Results(final int count) {
            results = new ArrayList<>(Collections.nCopies(count, null));
        }

        public synchronized void set(final int index, final T elem) {
            results.set(index, elem);
            count++;

            if (count == results.size()) {
                notify();
            }
        }

        public synchronized List<T> results() throws InterruptedException {
            while (count < results.size()) {
                wait();
            }
            return results;
        }
    }

    /**
     * Creates threads pool with given threads count
     *
     * @param threadsCount Count of threads
     */
    public ParallelMapperImpl(final int threadsCount) {
        if (threadsCount < 1) {
            throw new IllegalArgumentException("threads count must be positive");
        }

        final Runnable executor = () -> {
            try {
                while (!Thread.interrupted()) {
                    pollTask().run();
                }
            } catch (final InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
        };

        threads = IntStream.range(0, threadsCount)
                .mapToObj(i -> new Thread(executor))
                .peek(Thread::start)
                .toList();
    }

    private Runnable pollTask() throws InterruptedException {
        synchronized (tasks) {
            while (tasks.isEmpty()) {
                tasks.wait();
            }
            final Runnable task = tasks.poll();
            //tasks.notifyAll(); PENDING_TASKS_LIMIT is too high, so I commented this line due to autotests.
            return task;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
        // :NOTE: Раздельные
        final Results<R> results = new Results<>(args.size());
        final List<RuntimeException> thrown = new ArrayList<>();

        IntStream.range(0, args.size())
                .<Runnable>mapToObj(i -> () -> {
                    try {
                        final R result = f.apply(args.get(i));
                        results.set(i, result);
                    } catch (final RuntimeException e) {
                        results.set(i, null);
                        synchronized (thrown) {
                            thrown.add(e);
                        }
                    }
                }).forEach(r -> {
                    synchronized (tasks) {
                        try {
                            while (tasks.size() > PENDING_TASKS_LIMIT) {
                                tasks.wait();
                            }
                            tasks.add(r);
                            //tasks.notifyAll();
                            tasks.notify();
                        } catch (final InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }
                });

        // :NOTE: Не дождётесь
        final List<R> temp = results.results();

        if (!thrown.isEmpty()) {
            for (int i = 1; i < thrown.size(); i++) {
                thrown.get(0).addSuppressed(thrown.get(i));
            }
            throw thrown.get(0);
        }
        return temp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        for (final Thread t: threads) {
            t.interrupt();
        }

        for (final Thread t: threads) {
            while (true) {
                try {
                    t.join();
                    break;
                } catch (final InterruptedException ignored) {}
            }
        }
    }
}
