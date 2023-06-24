package solution;

import internal.Environment;

import java.util.HashSet;
import java.util.Set;

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Ilgiz Yakupov
 */
public class ProcessPhilosophers implements MutexProcess {
    private final Environment env;
    private final Set<Integer> cleanForks = new HashSet<>();
    private final Set<Integer> dirtyForks = new HashSet<>();
    private final Set<Integer> deferred = new HashSet<>();
    private enum Status {
        REQ, OK
    }
    private Boolean intentionToEnterCS = false;

    public ProcessPhilosophers(Environment env) {
        this.env = env;
        for (int i = 1; i < env.getProcessId(); i++) {
            dirtyForks.add(i);
        }
    }

    @Override
    public void onMessage(int sourcePid, Object message) {
        assert message instanceof Status;
        Status status = (Status) message;

        switch (status) {
            case REQ -> {
                if (dirtyForks.contains(sourcePid)) {
                    dirtyForks.remove(sourcePid);
                    env.send(sourcePid, Status.OK);
                    if (intentionToEnterCS) {
                        env.send(sourcePid, Status.REQ);
                    }
                } else {
                    deferred.add(sourcePid);
                }
            }
            case OK -> {
                assert !dirtyForks.contains(sourcePid) && !cleanForks.contains(sourcePid);
                cleanForks.add(sourcePid);

                if (dirtyForks.size() + cleanForks.size() == env.getNumberOfProcesses() - 1) {
                    cleanForks.addAll(dirtyForks);
                    dirtyForks.clear();
                    env.lock();
                    intentionToEnterCS = false;
                }
            }
            default -> throw new IllegalStateException();
        }
    }

    @Override
    public void onLockRequest() {
        boolean exclusive = true;
        intentionToEnterCS = true;
        for (int i = 1; i <= env.getNumberOfProcesses(); i++) {
            if (i != env.getProcessId() && !dirtyForks.contains(i) && !cleanForks.contains(i)) {
                env.send(i, Status.REQ);
                exclusive = false;
            }
        }
        if (exclusive) {
            cleanForks.addAll(dirtyForks);
            dirtyForks.clear();
            env.lock();
            intentionToEnterCS = false;
        }
    }

    @Override
    public void onUnlockRequest() {
        env.unlock();
        for (Integer i: cleanForks) {
            if (deferred.contains(i)) {
                deferred.remove(i);
                env.send(i, Status.OK);
            } else {
                dirtyForks.add(i);
            }
        }
        cleanForks.clear();
    }
}