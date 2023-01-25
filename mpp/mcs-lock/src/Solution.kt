import java.util.concurrent.atomic.*

class Solution(val env: Environment) : Lock<Solution.Node> {
    private val last = AtomicReference<Node>()

    override fun lock(): Node {
        val my = Node()
        this.last.getAndSet(my)?.let {
            it.next.set(my)
            while (my.locked.get()) env.park()
        }
        return my
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (last.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.get() == null) continue
            }
        }
        node.next.get().locked.set(false)
        env.unpark(node.next.get().thread)
    }

    class Node {
        val thread = Thread.currentThread()
        val locked = AtomicReference(true)
        val next = AtomicReference<Node>()
    }
}