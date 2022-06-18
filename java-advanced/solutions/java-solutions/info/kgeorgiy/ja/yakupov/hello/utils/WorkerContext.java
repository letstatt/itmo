package info.kgeorgiy.ja.yakupov.hello.utils;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.Deque;
import java.util.concurrent.Phaser;

import static java.nio.channels.SelectionKey.OP_WRITE;

/**
 * Class, providing tools for UDP server's requests/responses handling.
 */
public class WorkerContext implements Runnable {
    private final ByteBuffer request;
    private final ByteBuffer response;
    private final Deque<WorkerContext> completed;
    private final SelectionKey channelKey;
    private final byte[] placeholder;
    private SocketAddress sa;
    private boolean gotData;

    /**
     * Create class
     * @param requestBufCapacity Capacity for request buffer in bytes
     * @param responseBufCapacity Capacity for response buffer in bytes
     */
    public WorkerContext(final int requestBufCapacity,
                         final int responseBufCapacity,
                         final Deque<WorkerContext> completed,
                         final byte[] placeholder,
                         final SelectionKey channelKey) {
        // to avoid network stack allocations
        // request = ByteBuffer.allocateDirect(requestBufCapacity); // :NOTE: ??
        // response = ByteBuffer.allocateDirect(responseBufCapacity);
        request = ByteBuffer.allocate(requestBufCapacity);
        response = ByteBuffer.allocate(responseBufCapacity);
        this.completed = completed;
        this.placeholder = placeholder;
        this.channelKey = channelKey;
        sa = null;
    }

    @Override
    public void run() {
        while (!Thread.interrupted()) {
            try {
                ready();
            } catch (final IOException ignored) {}

            craftResponse(placeholder);
        }
    }

    /**
     * Receive request from given channel
     * @param channel DatagramChannel to give request from
     * @throws IOException Thrown in channel couldn't be read
     */
    public synchronized void receive(final DatagramChannel channel) throws IOException {
        request.clear();
        sa = channel.receive(request);
        request.flip();
        gotData = true;
        notify();
    }

    /**
     * Send response to given channel
     * @param channel DatagramChannel to send response to
     * @throws IOException Thrown if response couldn't be sent
     */
    public synchronized void send(final DatagramChannel channel) throws IOException {
        channel.send(response, sa);
    }

    /**
     * Checks if this context has prepared payload to send
     * @return True if some data could be sent
     */
    public synchronized boolean hasPayload() {
        return sa != null;
    }

    /**
     * Makes response to send back
     * @param placeholder Prefix + threadNumber + "_"
     */
    private synchronized void craftResponse(final byte[] placeholder) {
        response.clear();
        final int msgLength = Math.min(response.capacity() - placeholder.length, request.limit());

        try {
            request.limit(msgLength);
            response.put(placeholder);
            response.put(request);
            response.flip();
        } catch (final BufferOverflowException ignored) {
            sa = null;
        }
    }

    private synchronized void ready() throws IOException {
        gotData = false;
        synchronized (completed) {
            completed.add(this);
            if (completed.size() == 1) {
                channelKey.interestOpsOr(OP_WRITE);
                channelKey.selector().wakeup();
            }
        }
        while (!gotData) {
            try {
                wait();
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
}
