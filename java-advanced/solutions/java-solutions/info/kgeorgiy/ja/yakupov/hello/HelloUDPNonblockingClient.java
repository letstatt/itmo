package info.kgeorgiy.ja.yakupov.hello;

import info.kgeorgiy.ja.yakupov.hello.utils.AbstractUDPClient;
import info.kgeorgiy.ja.yakupov.hello.utils.ClientContext;
import info.kgeorgiy.ja.yakupov.hello.utils.Utils;
import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.Iterator;
import java.util.Set;

import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;

/**
 * Class providing requests sending to {@code HelloServer}
 * @see HelloServer
 */
public class HelloUDPNonblockingClient extends AbstractUDPClient implements HelloClient {

    /**
     * Default constructor. Do nothing.
     */
    public HelloUDPNonblockingClient() {}

    /**
     * Create class with given timeout to receive
     * @param receiveTimeout Timeout to receive
     */
    public HelloUDPNonblockingClient(final int receiveTimeout) {
        super(receiveTimeout);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        final SocketAddress sa = Utils.getSocketAddress(host, port);

        final DatagramChannel[] channels = new DatagramChannel[threads];
        Selector selector = null;

        int requestBufferSize = Integer.MAX_VALUE;
        int responseBufferSize = 0;

        try {
            selector = Selector.open();

            for (int i = 0; i < threads; i++) {
                final DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.connect(sa);

                final int requestSize = channel.getOption(StandardSocketOptions.SO_SNDBUF);
                final int responseSize = channel.getOption(StandardSocketOptions.SO_RCVBUF);

                requestBufferSize = Math.min(requestBufferSize, requestSize);
                responseBufferSize = Math.max(responseBufferSize, responseSize);

                final ClientContext ctx = new ClientContext(
                        prefix, i, requests, requestSize, responseSize);

                channel.register(selector, OP_WRITE, ctx);
                channels[i] = channel;
            }

            // to avoid network stack allocations
            //final ByteBuffer request = ByteBuffer.allocateDirect(requestBufferSize);
            //final ByteBuffer response = ByteBuffer.allocateDirect(responseBufferSize);
            final ByteBuffer request = ByteBuffer.allocate(requestBufferSize);
            final ByteBuffer response = ByteBuffer.allocate(responseBufferSize);

            try {
                clientRunner(selector, request, response);
            } catch (final IOException e) {
                Utils.silentClose(selector);
                Utils.silentClose(channels);
                throw new RuntimeException("An error occurred during the work", e);
            }
        } catch (final IOException e) {
            Utils.silentClose(selector);
            Utils.silentClose(channels);
            throw new RuntimeException("Initialization error", e);
        }
    }

    private void clientRunner(
            final Selector selector,
            final ByteBuffer request,
            final ByteBuffer response
    ) throws IOException {

        while (!selector.keys().isEmpty()) {
            selector.select(receiveTimeout);

            final Set<SelectionKey> selectionKeys = selector.selectedKeys();
            final Iterator<SelectionKey> keyIter = selectionKeys.iterator();

            // fallback (case: no one answered)
            if (selectionKeys.isEmpty()) {
                for (final SelectionKey key : selector.keys()) {
                    key.interestOps(OP_WRITE);
                    ((ClientContext) key.attachment()).failed();
                }
            } else {
                while (keyIter.hasNext()) {
                    final SelectionKey key = keyIter.next();
                    final DatagramChannel channel = (DatagramChannel) key.channel();
                    final ClientContext ctx = (ClientContext) key.attachment();

                    try {
                        // check if one "thread" is done
                        if (ctx.requestsEnded()) {
                            Utils.silentClose(channel);
                            key.cancel();
                            continue;
                        }

                        if (key.isReadable()) {
                            handleReading(response, key, channel, ctx);
                        } else if (key.isWritable()) {
                            handleWriting(request, key, channel, ctx);
                        }

                    } catch (final IOException e) {
                        ctx.failed();
                    } finally {
                        keyIter.remove();
                    }
                }
            }
        }
    }

    private static void handleWriting(
            final ByteBuffer request,
            final SelectionKey key,
            final WritableByteChannel channel,
            final ClientContext ctx) throws IOException {

        ctx.encode(request);
        channel.write(request);
        key.interestOps(OP_READ);
    }

    private static void handleReading(
            final ByteBuffer response,
            final SelectionKey key,
            final ReadableByteChannel channel,
            final ClientContext ctx
    ) throws IOException {

        response.clear();
        channel.read(response);
        response.flip();

        ctx.check(response);
        key.interestOps(OP_WRITE);
    }

    /**
     * Start CLI application
     * @param args command-line args
     */
    public static void main(final String[] args) {
        cli(new HelloUDPNonblockingClient(), args);
    }
}
