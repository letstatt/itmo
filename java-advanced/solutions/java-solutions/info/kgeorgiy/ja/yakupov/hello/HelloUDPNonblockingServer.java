package info.kgeorgiy.ja.yakupov.hello;

import info.kgeorgiy.ja.yakupov.hello.utils.AbstractUDPServer;
import info.kgeorgiy.ja.yakupov.hello.utils.Utils;
import info.kgeorgiy.ja.yakupov.hello.utils.WorkerContext;
import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.InetSocketAddress;
import java.nio.channels.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.stream.IntStream;

import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;

/**
 * Echo server.
 * @see HelloClient
 */
public class HelloUDPNonblockingServer extends AbstractUDPServer implements HelloServer {
    private Selector selector;
    private DatagramChannel channel;
    private Deque<WorkerContext> free;
    private Deque<WorkerContext> completed;
    private ExecutorService workers;
    private boolean started = false;

    /**
     * Default constructor.
     */
    public HelloUDPNonblockingServer() {}

    @Override
    public void start(final int port, final int threads) {
        if (started) {
            throw new IllegalStateException("Server has already been started");
        }
        started = true;

        try {
            selector = Selector.open();
            channel = DatagramChannel.open();

            channel.configureBlocking(false);
            channel.bind(new InetSocketAddress(port));

            final SelectionKey channelKey = channel.register(selector, 0);
            final int recvBufCapacity = channel.socket().getReceiveBufferSize();
            final int sendBufCapacity = channel.socket().getSendBufferSize();

            free = new ArrayDeque<>(threads);
            completed = new ArrayDeque<>(threads);
            workers = Executors.newFixedThreadPool(threads + 1);

            for (int i = 0; i < threads; i++) {
                workers.submit(new WorkerContext(
                        recvBufCapacity,
                        sendBufCapacity,
                        completed,
                        placeholder,
                        channelKey
                ));
            }

            workers.submit(this::controller);

        } catch (final IOException e) {
            System.err.println("Unable to start server: " + e.getMessage());
        }
    }

    private void controller() {
        try {
            while (!Thread.interrupted()) {
                selector.select();
                final Set<SelectionKey> keys = selector.selectedKeys();

                for (final SelectionKey key: keys) {
                    if (key.isReadable()) {
                        handleSocketRead(key);
                    }
                    if (key.isWritable()) {
                        handleSocketWrite(key);
                    }
                }
                keys.clear();
            }

        } catch (final ClosedSelectorException e) {
            System.out.println("Selector closed, shutting down...");

        } catch (final IOException e) {
            System.err.println("Controller got an error, shutting down: " + e.getMessage());

        } finally {
            Utils.silentClose(selector);
        }
    }

    private void handleSocketRead(final SelectionKey key) {
        final WorkerContext context = free.poll();

        try {
            // read from channel and resume worker
            context.receive(channel);

            if (free.isEmpty()) {
                key.interestOpsAnd(~OP_READ);
            }
        } catch (final AsynchronousCloseException e) {
            // shutdown
        } catch (final IOException e) {
            System.err.println("Channel reading failed");
            free.add(context);
        }
    }

    private void handleSocketWrite(final SelectionKey key) {
        final WorkerContext context;

        synchronized (completed) {
            context = completed.poll();
        }

        if (context.hasPayload()) {
            try {
                // send response through DatagramChannel
                context.send(channel);
            } catch (final AsynchronousCloseException e) {
                return; // shutdown
            } catch (final IOException e) {
                System.err.println("Sending response failed");
            }
        }

        free.add(context);
        if (free.size() == 1) {
            key.interestOpsOr(OP_READ);
        }
        synchronized (completed) {
            if (completed.isEmpty()) {
                key.interestOpsAnd(~OP_WRITE);
            }
        }
    }

    @Override
    public void close() {
        Utils.silentClose(selector);
        if (workers != null) {
            Utils.shutdownExecutorService(workers, true);
        }
        Utils.silentClose(channel);
    }

    /**
     * Start CLI application
     * @param args command-line args
     */
    public static void main(final String[] args) {
        cli(HelloUDPNonblockingServer.class, args);
    }
}
