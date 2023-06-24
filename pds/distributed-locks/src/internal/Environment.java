package internal;

import solution.MutexProcess;

/**
 * Environment interface for communication with other processes.
 */
public interface Environment {
    /**
     * Identifier of this process (from 1 to {@link #getNumberOfProcesses()}).
     */
    int getProcessId();

    /**
     * The total number of processes in the system.
     */
    int getNumberOfProcesses();

    /**
     * Indicates that the process had entered critical section.
     * It should be only called after locking was requested via {@link MutexProcess#onLockRequest()}.
     */
    void lock();

    /**
     * Indicates that the process had exited critical section.
     * It should be only called after unlocking was requested via {@link MutexProcess#onUnlockRequest()}.
     */
    void unlock();

    /**
     * Sends the specified message to the process destinationPid (from 1 to {@link #getNumberOfProcesses()}).
     */
    void send(int destinationPid, Object message);
}
