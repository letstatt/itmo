package info.kgeorgiy.ja.yakupov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.BiConsumer;
import java.util.function.Predicate;

public class WebCrawler implements Crawler {
    private final ExecutorService downloaders; // internal queue is unbound
    private final ExecutorService extractors; // internal queue is unbound
    private final Scheduler scheduler;
    private final Downloader downloader;

    private class Scheduler {
        private final ConcurrentMap<String, HostQueue> hosts;
        private final int perHost;

        private class HostQueue {
            private static final int INITIAL_CAPACITY = 200;
            private final Queue<Runnable> queue = new ArrayDeque<>(INITIAL_CAPACITY);
            private int inProgress = 0;

            public synchronized void put(final Runnable task) {
                if (inProgress < perHost) {
                    downloaders.submit(task);
                    inProgress++;
                } else {
                    queue.add(task);
                }
            }

            public synchronized void tryNext() {
                if (!queue.isEmpty()) {
                    downloaders.submit(queue.poll());
                } else {
                    inProgress--;
                }
            }
        }

        /**
         * Creates Scheduler instance.
         * Scheduler limits web pages downloading from the same host in the same time by {@code perHost} number.
         * @see URLUtils#getHost(String)
         * @param perHost Max count of concurrent requests to the same host
         */
        Scheduler(final int perHost) {
            hosts = new ConcurrentHashMap<>();
            this.perHost = perHost;
        }

        /**
         * Queue a web page downloading with respect to {@code perHost} limit.
         * @param url Url to be downloaded and parsed
         * @param onDone Handler for url processing completion
         * @param used Thread-safe collection for links which were visited or planned
         * @param frontier Thread-safe collection for extracted links (or null if it doesn't need)
         */
        void put(
                final String url,
                final BiConsumer<String, IOException> onDone,
                final Set<String> used,
                final Queue<String> frontier,
                final Phaser phaser
        ) {
            if (!used.add(url)) {
                // already processed.
                // there's also "finally" block.
                return;
            }

            final String host;

            try {
                host = URLUtils.getHost(url);
            } catch (final MalformedURLException e) {
                onDone.accept(url, e);
                return;
            }

            final HostQueue queue = hosts.computeIfAbsent(host, s -> new HostQueue());

            final Runnable task = () -> {
                try {
                    // try to download
                    extract(url, onDone, used, frontier, phaser, downloader.download(url));
                } catch (final IOException e) {
                    onDone.accept(url, e);
                } finally {
                    // :NOTE: -> HostQueue
                    queue.tryNext();
                    phaser.arriveAndDeregister(); // downloaded
                }
            };

            try {
                queue.put(task);
            } finally {
                phaser.register(); // submitted to downloaders
            }
        }

        private void extract(
                final String url,
                final BiConsumer<String, IOException> onDone,
                final Set<String> used,
                final Queue<String> frontier,
                final Phaser phaser,
                final Document doc
        ) {
            if (frontier != null) {
                // fill frontier layer in parallel
                extractors.submit(() -> {
                    try {
                        doc.extractLinks().parallelStream()
                                .filter(Predicate.not(used::contains))
                                .forEach(frontier::add);
                        onDone.accept(url, null);
                    } catch (final IOException e) {
                        onDone.accept(url, e);
                    } finally {
                        phaser.arriveAndDeregister(); // extracted
                    }
                });
                phaser.register(); // submitted to extractors
            } else {
                onDone.accept(url, null);
            }
        }
    }

    /**
     * Creates WebCrawler instance.
     * WebCrawler let you traverse websites recursively.
     * @param downloader Provides a method to download web pages by url with extracting links support
     * @param downloaders Max count of threads that could be used for downloading
     * @param extractors Max count of threads that could be used for links extracting
     * @param perHost Max count of concurrent requests to the same host
     */
    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.scheduler = new Scheduler(perHost);
        this.downloader = downloader;
    }

    // javadoc inherited
    @Override
    public Result download(final String url, final int depth) {
        final ConcurrentMap<String, IOException> errors = new ConcurrentHashMap<>();
        final List<String> succeed = new ArrayList<>(1024);
        final Set<String> used = new ConcurrentSkipListSet<>();

        Collection<String> layer = List.of(url);

        final Phaser phaser = new Phaser(1);
        final BiConsumer<String, IOException> onDone = (u, e) -> {
            if (e != null) {
                errors.put(u, e);
            } else {
                synchronized (succeed) {
                    succeed.add(u);
                }
            }
        };

        for (int i = 0; i < depth; i++) {
            final Queue<String> frontier = (i + 1 < depth ? new ConcurrentLinkedQueue<>() : null);

            layer.forEach(s -> scheduler.put(s, onDone, used, frontier, phaser));
            phaser.arriveAndAwaitAdvance();
            layer = frontier;
        }

        return new Result(succeed, errors);
    }

    // javadoc inherited
    @Override
    public void close() {
        downloaders.shutdownNow();
        extractors.shutdownNow();

        while (true) {
            try {
                if (downloaders.awaitTermination(1L, TimeUnit.MINUTES)
                        || extractors.awaitTermination(1L, TimeUnit.MINUTES)) {
                    break;
                }
            } catch (final InterruptedException ignored) {}
        }
    }

    /**
     * Start console application for website crawling.
     * Prints successfully visited and errored urls to {@code stdout}.
     * @see WebCrawler
     * @param args Array with string representations of necessary and optional arguments.<br/>
     *             {@code args[0]} is the entry point for crawling (url, necessary argument)<br/>
     *             {@code args[1]} is max depth for crawling (positive int, optional argument)<br/>
     *             {@code args[2]} is max count of parallel downloading threads (positive int, optional argument)<br/>
     *             {@code args[3]} is max count of parallel extracting threads (positive int, optional argument)<br/>
     *             {@code args[4]} is max count of parallel requests to the same host (positive int, optional argument)
     */
    public static void main(final String[] args) {
        if (args == null || args.length == 0 || args.length > 5
                || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println("Usage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
            return;
        }

        final int[] numericArgs = new int[4];
        final CachingDownloader cd;

        Arrays.fill(numericArgs, 4);

        try {
            for (int i = 1; i < args.length; i++) {
                numericArgs[i - 1] = Math.max(1, Integer.parseUnsignedInt(args[i]));
            }
            cd = new CachingDownloader();

        } catch (final NumberFormatException e) {
            System.err.println("Optional arguments are expected to be positive integers");
            return;

        } catch (final IOException e) {
            System.err.println(e.getMessage());
            return;
        }

        try (final WebCrawler wb = new WebCrawler(cd, numericArgs[1], numericArgs[2], numericArgs[3])) {
            final Result result = wb.download(args[0], numericArgs[0]);

            System.out.println("Succeeded:");
            for (final String url: result.getDownloaded()) {
                System.out.println(url);
            }
            System.out.println("Errored:");
            for (final Map.Entry<String, IOException> entry: result.getErrors().entrySet()) {
                System.out.println(entry.getKey() + entry.getValue().getMessage());
            }
        }
    }
}
