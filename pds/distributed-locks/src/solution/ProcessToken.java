package solution;

import internal.Environment;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class ProcessToken implements MutexProcess {
    private final Environment env;
    private boolean wantToEnter = false;

    public ProcessToken(Environment env) {
        this.env = env;
        if (env.getProcessId() == 1) {
            env.send(env.getProcessId() + 1, null);
        }
    }

    @Override
    public void onMessage(int sourcePid, Object message) {
        if (wantToEnter) {
            env.lock();
        } else {
            releaseToken();
        }
    }

    @Override
    public void onLockRequest() {
        wantToEnter = true;
    }

    @Override
    public void onUnlockRequest() {
        wantToEnter = false;
        env.unlock();
        releaseToken();
    }

    private void releaseToken() {
        int nextPid = (env.getProcessId() == env.getNumberOfProcesses()) ? 1 : env.getProcessId() + 1;
        env.send(nextPid, null);
    }
}
