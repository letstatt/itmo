import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

/**
 * An element is transferred from sender to receiver only when [send] and [receive]
 * invocations meet in time (rendezvous), so [send] suspends until another coroutine
 * invokes [receive] and [receive] suspends until another coroutine invokes [send].
 */
class SynchronousQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(false)
        head = atomic(dummy)
        tail = atomic(dummy)
    }
    /**
     * Sends the specified [element] to this channel, suspending if there is no waiting
     * [receive] invocation on this channel.
     */
    suspend fun send(element: E): Unit {
        val node = Node<E>(true)
        node.x.value = element

        while (true) {
            val last = tail.value
            val first = head.value
            val next = last.next.value

            if (next == first) {
                if (tail.compareAndSet(last, next)) {
                    continue // duct tape :(
                }
            }

            if (last == first || last.isSender) {
                if (enqueueAndSuspend(last, node)) {
                    break
                }
            } else {
                val next = first.next.value ?: continue // no elements :(
                if (!next.isSender && head.compareAndSet(first, next)) {
                    next.x.value = element
                    next.cont!!.resume(true) // rendezvous!
                    return
                }
            }
        }
    }

    private suspend fun enqueueAndSuspend(last: Node<E>, node: Node<E>): Boolean {
        val res = suspendCoroutine sc@ { cont ->
            node.cont = cont
            if (last.next.compareAndSet(null, node)) {
                tail.compareAndSet(last, node)
                // if last CAS failed, someone helped us
            } else {
                // help and repeat
                last.next.value?.let { tail.compareAndSet(last, it) }
                cont.resume(false)
            }
        }
        return res
    }

    /**
     * Retrieves and removes an element from this channel if there is a waiting [send] invocation on it,
     * suspends the caller if this channel is empty.
     */
    suspend fun receive(): E {
        val node = Node<E>(false)

        while (true) {
            val last = tail.value
            val first = head.value
            val next = last.next.value

            if (next == first) {
                if (tail.compareAndSet(last, next)) {
                    continue // duct tape :(
                }
            }

            if (last == first || !last.isSender) {
                if (enqueueAndSuspend(last, node)) {
                    return node.x.value!!
                }
            } else {
                val next = first.next.value ?: continue // no elements :(
                if (next.isSender && head.compareAndSet(first, next)) {
                    next.cont!!.resume(true) // rendezvous!
                    return next.x.value!!
                }
            }
        }
    }
}

private class Node<E>(val isSender: Boolean) {
    val next = atomic<Node<E>?>(null)
    var cont: Continuation<Boolean>? = null
    val x: AtomicRef<E?> = atomic(null)
}
