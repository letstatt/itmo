package solution;

import internal.Environment;

/**
 * Interface that process implementing mutual exclusion algorithm shall satisfy.
 * All functions are called from the single main thread.
 */
public interface MutexProcess {
    /**
     * Called on arriving message from another process with sourcePid (from 1 to {@link Environment#getNumberOfProcesses()}).
     */
    void onMessage(int sourcePid, Object message);

    /**
     * Called on lock request.
     */
    void onLockRequest();

    /**
     * Called on unlock request.
     */
    void onUnlockRequest();
}
