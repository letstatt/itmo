package solution;

import internal.Environment;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.util.*;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class ProcessLamport implements MutexProcess {
    private final Environment env;
    private int time;
    private int oks;
    private static class MutexRequest implements Comparable<MutexRequest> {
        int id;
        int time;
        MutexRequest(int id, int time) {
            this.id = id;
            this.time = time;
        }

        @Override
        public int compareTo(@NotNull MutexRequest o) {
            if (this.time < o.time) return -1;
            if (this.time > o.time) return 1;
            return this.id - o.id;
        }
    }
    private final Queue<MutexRequest> q;
    private enum Status {
        REQ, OK, REL
    }
    private static final Status[] StatusMap = {Status.REQ, Status.OK, Status.REL};
    private record Message(Status status, int reqTime, int senderTime) implements Serializable {}

    public ProcessLamport(Environment env) {
        q = new PriorityQueue<>();
        this.env = env;
    }

    @Override
    public void onMessage(int sourcePid, Object message) {
        Message m = receiveMessage(message);
        updateTime(Math.max(getTime(), m.senderTime) + 1); // update Lamport's time

        switch (m.status) {
            case REQ -> {
                q.add(new MutexRequest(sourcePid, m.reqTime));
                sendStatus(sourcePid, Status.OK, getTime());
            }
            case REL -> {
                for (MutexRequest req: q) {
                    if (req.id == sourcePid) {
                        q.remove(req);
                        break;
                    }
                }
                if (!q.isEmpty() && q.peek().id == env.getProcessId() && oks == env.getNumberOfProcesses()) {
                    env.lock();
                }
            }
            case OK -> {
                oks++;
                if (!q.isEmpty() && q.peek().id == env.getProcessId() && oks == env.getNumberOfProcesses()) {
                    env.lock();
                }
            }
            default -> throw new IllegalStateException();
        }
    }

    @Override
    public void onLockRequest() {
        if (env.getNumberOfProcesses() == 1) {
            env.lock();
        } else {
            oks = 1;
            q.add(new MutexRequest(env.getProcessId(), getTime()));
            shareStatus(Status.REQ, getTime());
        }
    }

    @Override
    public void onUnlockRequest() {
        if (env.getNumberOfProcesses() == 1) {
            env.unlock();
        } else {
            assert (!q.isEmpty());
            if (q.peek().id != env.getProcessId()) {
                throw new IllegalStateException();
            }

            env.unlock();
            q.remove();
            shareStatus(Status.REL, getTime());
        }
    }

    private int getTime() {
        return time;
    }

    private void updateTime(int newTime) {
        assert(newTime > getTime());
        time = newTime;
    }

    private void shareStatus(Status status, int fixedTime) {
        for (int i = 1; i <= env.getNumberOfProcesses(); i++) {
            if (i == env.getProcessId()) {
                continue;
            }
            sendStatus(i, status, fixedTime);
        }
    }

    private void sendStatus(int processId, Status status, int fixedTime) {
        int time = getTime() + 1;
        env.send(processId, encodeMessage(new Message(status, fixedTime, time)));
        updateTime(time);
    }

    private Message receiveMessage(Object message) {
        return decodeMessage(message);
    }

    private Object encodeMessage(Message m) {
        return new int[]{m.status.ordinal(), m.reqTime, m.senderTime};
    }

    private Message decodeMessage(Object m) {
        int[] arr = (int[]) m;
        return new Message(StatusMap[arr[0]], arr[1], arr[2]);
    }
}
