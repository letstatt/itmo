package solution;

import internal.Environment;
import internal.Pair;

import java.util.Map;

/**
 * Distributed Dijkstra algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class DijkstraProcessImpl implements DijkstraProcess {
    private final Environment env;
    private int waitingForAck = 0;
    private int childrenCount = 0;
    private long distance = -1;
    private int parentId = -1;
    enum Status {
        REQ, ACK, CHILD_LINK, CHILD_UNLINK
    }
    private static final Status[] statusValues = Status.values();

    public DijkstraProcessImpl(Environment env) {
        this.env = env;
    }

    @Override
    public void onMessage(int senderPid, Object message) {
        var msg = decodeMessage(message);
        long distance = msg.first();
        Status status = msg.second();

        switch (status) {
            case REQ -> {
                assert distance >= 0;
                boolean sent = false;

                if (this.distance > distance || this.distance == -1) {
                    this.distance = distance;
                    sent = updateNeighbors(senderPid);
                }

                if (sent) {
                    if (parentId == -1) {
                        parentId = senderPid;
                        env.send(senderPid, encodeMessage(Status.CHILD_LINK));
                    } else {
                        env.send(senderPid, encodeMessage(Status.ACK));
                    }
                } else {
                    env.send(senderPid, encodeMessage(Status.ACK));
                }
            }
            case ACK -> {
                waitingForAck -= 1;
                tryToLeaveSpanningTree();
            }
            case CHILD_LINK -> {
                childrenCount += 1;
                waitingForAck -= 1;
            }
            case CHILD_UNLINK -> {
                childrenCount -= 1;
                tryToLeaveSpanningTree();
            }
        }
    }

    @Override
    public Long getDistance() {
        return this.distance >= 0 ? this.distance : null;
    }

    @Override
    public void onComputationStart() {
        parentId = -2;
        distance = 0;
        updateNeighbors(-1);
        tryToLeaveSpanningTree();
    }

    private boolean updateNeighbors(int except) {
        boolean sent = false;
        for (Map.Entry<Integer, Long> entry: env.getNeighbours().entrySet()) {
            int neighborId = entry.getKey();
            long distance = entry.getValue();
            if (neighborId == env.getProcessId() || neighborId == except) {
                continue;
            }
            sent = true;
            waitingForAck += 1;
            env.send(neighborId, encodeMessage(this.distance + distance, Status.REQ));
        }
        return sent;
    }

    private void tryToLeaveSpanningTree() {
        if (waitingForAck == 0 && childrenCount == 0) {
            if (parentId == -2) {
                env.finishExecution();
            } else {
                env.send(parentId, encodeMessage(Status.CHILD_UNLINK));
                parentId = -1;
            }
        }
    }

    private static Object encodeMessage(Status status) {
        return encodeMessage(-1, status);
    }

    private static Object encodeMessage(long distance, Status status) {
        return new long[]{distance, status.ordinal()};
    }

    private Pair<Long, Status> decodeMessage(Object message) {
        var msg = (long[]) message;
        return new Pair<>(msg[0], statusValues[(int) msg[1]]);
    }
}
