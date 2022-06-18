package info.kgeorgiy.ja.yakupov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Class, providing common patterns for parallel {@code List} processing
 *
 */
public class IterativeParallelism implements ListIP {
    private final ParallelMapper mapper;

    /**
     * Creates default {@code IterativeParallelism} instance
     */
    public IterativeParallelism() {
        mapper = null;
    }

    /**
     * Creates {@code IterativeParallelism} instance which use given threads pool
     *
     * @param mapper A {@code ParallelMapper} instance
     */
    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public String join(final int threads, final List<?> values) throws InterruptedException {
        return streamProcessing(threads, values, s -> s.map(String::valueOf), Collectors.joining());
    }

    @Override
    public <T> List<T> filter(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return streamProcessing(threads, values, s -> s.filter(predicate), Collectors.toList());
    }

    @Override
    public <T, U> List<U> map(
            final int threads,
            final List<? extends T> values,
            final Function<? super T, ? extends U> f
    ) throws InterruptedException {
        return streamProcessing(threads, values, s -> s.map(f), Collectors.toList());
    }

    @Override
    public <T> T maximum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return runThreads(threads, values, s -> s.max(comparator).orElseThrow(), s -> s.max(comparator).orElseThrow());
    }

    @Override
    public <T> T minimum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return runThreads(threads, values, s -> s.min(comparator).orElseThrow(), s -> s.min(comparator).orElseThrow());
    }

    @Override
    public <T> boolean all(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return runThreads(threads, values, s -> s.allMatch(predicate), s -> s.allMatch(Boolean::booleanValue));
    }

    @Override
    public <T> boolean any(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return runThreads(threads, values, s -> s.anyMatch(predicate), s -> s.anyMatch(Boolean::booleanValue));
    }

    private <T, A, U> U streamProcessing(
            final int threads,
            final List<? extends T> values,
            final Function<Stream<? extends T>, Stream<? extends A>> streamProcessor,
            final Collector<A, ?, U> c
    ) throws InterruptedException {
        return runThreads(threads, values, s -> streamProcessor.apply(s).toList(),
                s -> s.flatMap(Collection::stream).collect(c));
    }

    private <T, S, R> R runThreads(
            final int maxThreads, final List<? extends T> values,
            final Function<Stream<? extends T>, ? extends S> pipeProcessor,
            final Function<Stream<? extends S>, R> joiner
    ) throws InterruptedException {

        if (maxThreads <= 0) {
            throw new IllegalArgumentException("Number of threads must be > 0");
        }

        final List<S> results;

        if (!values.isEmpty()) {
            final int threads = Math.min(maxThreads, values.size());
            final List<List<? extends T>> tasks = splitTasks(values, threads);

            results = mapper != null ?
                    mapper.map(l -> pipeProcessor.apply(l.stream()), tasks) : map(pipeProcessor, threads, tasks);
        } else {
            results = List.of();
        }

        // JOIN RESULTS AND EXIT

        return joiner.apply(results.parallelStream());
    }

    private static <T, S> List<S> map(
            final Function<Stream<? extends T>, ? extends S> pipeProcessor,
            final int threads,
            final List<List<? extends T>> tasks
    ) throws InterruptedException {
        final List<S> results = new ArrayList<>(Collections.nCopies(threads, null));

        // START WORKERS

        final List<Thread> workers = IntStream.range(0, threads)
                .mapToObj(i -> new Thread(() -> {
                    final S result = pipeProcessor.apply(tasks.get(i).stream());
                    results.set(i, result);
                }))
                .peek(Thread::start)
                .toList();

        // JOIN THREADS

        final InterruptedException[] exception = {null};

        workers.forEach(w -> {
            while (true) {
                try {
                    w.join();
                    break;
                } catch (final InterruptedException e) {
                    if (exception[0] != null) {
                        exception[0].addSuppressed(e);
                    } else {
                        exception[0] = e;
                    }
                }
            }
        });

        if (exception[0] != null) {
            throw exception[0];
        }

        return results;
    }

    private static <T> List<List<? extends T>> splitTasks(final List<? extends T> values, final int threads) {
        final int n = values.size();
        final int m = n / threads;
        final int rem = n - m * threads;

        final List<List<? extends T>> tasks = new ArrayList<>(m);

        for (int i = 0, start = 0; i < threads; i++) {
            final int end = start + m + (i < rem ? 1 : 0);
            tasks.add(values.subList(start, end)); // O(1) - subList gets views, not copies
            start = end;
        }
        return tasks;
    }
}
