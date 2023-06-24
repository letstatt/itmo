package solution;

import internal.Environment;

import java.util.ArrayDeque;
import java.util.Queue;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class ProcessCentralized implements MutexProcess {
    private final Environment env;
    private final Queue<Integer> q = new ArrayDeque<>();
    private enum Status {
        REQ, OK, REL
    }

    public ProcessCentralized(Environment env) {
        this.env = env;
    }

    @Override
    public void onMessage(int sourcePid, Object message) {
        assert(message instanceof Status);

        if (sourcePid == 1) {
            if (message == Status.OK) {
                env.lock();
            } else {
                throw new IllegalStateException();
            }
        } else if (env.getProcessId() == 1) {
            switch ((Status) message) {
                case REQ -> doLock(sourcePid);
                case REL -> doUnlock();
                default -> throw new IllegalStateException();
            }
        }
    }

    @Override
    public void onLockRequest() {
        if (env.getProcessId() == 1) {
            doLock(1);
        } else {
            env.send(1, Status.REQ);
        }
    }

    @Override
    public void onUnlockRequest() {
        env.unlock();
        if (env.getProcessId() == 1) {
            doUnlock();
        } else {
            env.send(1, Status.REL);
        }
    }

    private synchronized void doLock(int processId) {
        assert(env.getProcessId() == 1);

        q.add(processId);
        if (q.size() == 1) {
            if (processId == 1) {
                env.lock();
            } else {
                env.send(processId, Status.OK);
            }
        }
    }

    private synchronized void doUnlock() {
        assert(env.getProcessId() == 1);

        q.remove();
        if (!q.isEmpty()) {
            var next = q.peek();
            if (next == 1) {
                env.lock();
            } else {
                env.send(next, Status.OK);
            }
        }
    }
}
