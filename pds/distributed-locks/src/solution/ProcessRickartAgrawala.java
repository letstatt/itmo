package solution;

import internal.Environment;

import java.util.*;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class ProcessRickartAgrawala implements MutexProcess {
    private enum Status {
        REQ, OK
    }
    private final Status[] StatusMap = {Status.REQ, Status.OK};
    private record Message(Status status, int reqTime, int senderTime) {}
    private final Boolean[] deferred;
    private final Environment env;
    private enum State {
        IDLE, REQUESTING, MUTEX
    }
    private State state;
    private int reqTime;
    private int oks;
    private int time;

    public ProcessRickartAgrawala(Environment env) {
        deferred = new Boolean[env.getNumberOfProcesses() + 1];
        Arrays.fill(deferred, false);
        state = State.IDLE;
        this.env = env;
    }

    @Override
    public void onMessage(int sourcePid, Object message) {
        Message m = getMessage(message);
        updateTime(Math.max(getTime(), m.senderTime) + 1);

        switch (m.status) {
            case REQ -> {
                if (state == State.IDLE || state == State.REQUESTING &&
                        !(reqTime < m.reqTime || reqTime == m.reqTime && env.getProcessId() < sourcePid)) {
                    sendMessage(sourcePid, new Message(Status.OK, reqTime, 0));
                } else {
                    deferred[sourcePid] = true;
                }
            }
            case OK -> {
                oks++;
                if (oks == env.getNumberOfProcesses()) {
                    state = State.MUTEX;
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
            state = State.REQUESTING;
            int reqTime = getTime();
            this.reqTime = reqTime;
            oks = 1;

            for (int i = 1; i <= env.getNumberOfProcesses(); i++) {
                if (i == env.getProcessId()) {
                    continue;
                }
                sendMessage(i, new Message(Status.REQ, reqTime, 0));
            }
        }
    }

    @Override
    public void onUnlockRequest() {
        if (env.getNumberOfProcesses() == 1) {
            env.unlock();
        } else {
            env.unlock();
            state = State.IDLE;
            oks = 0;

            for (int i = 1; i <= env.getNumberOfProcesses(); i++) {
                if (i == env.getProcessId()) {
                    continue;
                }
                if (deferred[i]) {
                    deferred[i] = false;
                    sendMessage(i, new Message(Status.OK, reqTime, 0));
                }
            }
        }
    }

    private int getTime() {
        return time;
    }

    private void updateTime(int newTime) {
        assert newTime > time;
        time = newTime;
    }

    private void sendMessage(int dest, Message msg) {
        int time = getTime() + 1;
        env.send(dest, new int[]{msg.status.ordinal(), msg.reqTime, time});
        updateTime(time);
    }

    private Message getMessage(Object msg) {
        int[] arr = (int[]) msg;
        return new Message(StatusMap[arr[0]], arr[1], arr[2]);
    }
}
