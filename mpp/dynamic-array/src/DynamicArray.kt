package mpp.dynamicarray

import kotlinx.atomicfu.*

interface DynamicArray<E> {
    /**
     * Returns the element located in the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun get(index: Int): E

    /**
     * Puts the specified [element] into the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun put(index: Int, element: E)

    /**
     * Adds the specified [element] to this array
     * increasing its [size].
     */
    fun pushBack(element: E)

    /**
     * Returns the current size of this array,
     * it increases with [pushBack] invocations.
     */
    val size: Int
}

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))

    override fun get(index: Int): E = core.value.get(index)

    override fun put(index: Int, element: E) {
        core.value.put(index, element)
    }

    override fun pushBack(element: E) {
        while (true) {
            val c = core.value
            var ptr = c._size.value
            val capacity = c.capacity
            while (ptr < capacity) {
                if (c.array[ptr].compareAndSet(null, element)) {
                    c._size.compareAndSet(ptr, ptr + 1)
                    return // good at you
                } else {
                    c._size.compareAndSet(ptr, ptr + 1) // helping
                }
                ptr += 1
            }
            // otherwise we are out of bounds. try to expand
            val newCore = Core<E>(capacity * 2, ptr + 1)
            newCore.compareAndSet(ptr, null, element)

            val moving : (Core<E>) -> Unit = {c2: Core<E> ->
                for (i in 0 until ptr) {
                    while (true) {
                        val e = c.array[i].value
                        if (e is Moved<*>) {
                            c2.compareAndSet(i, null, e.x) // helping
                            break
                        }
                        if (c.array[i].compareAndSet(e, Moved(e))) {
                            c2.compareAndSet(i, null, e) // doesn't matter if failed
                            break
                        }
                    }
                }
                core.compareAndSet(c, c2)
            }

            if (!c.next.compareAndSet(null, newCore)) {
                moving(c.next.value!!) // helping
            } else {
                moving(newCore)
                return // success
            }
        }
    }

    override val size: Int get() = core.value.size
}

private class Moved<E>(val x: E)

private class Core<E>(
    val capacity: Int,
    size: Int = 0
) {
    val array = atomicArrayOfNulls<Any>(capacity)
    val _size = atomic(size)
    val next : AtomicRef<Core<E>?> = atomic(null)

    val size: Int
        get() {
            return _size.value
        }

    @Suppress("UNCHECKED_CAST")
    fun get(index: Int): E {
        require(index < size)
        var c = this
        var prev : E? = null
        while (true) {
            val elem = c.array[index].value
            if (elem is Moved<*>) {
                prev = elem.x as E
                c = c.next.value!!
            } else if (elem == null) {
                require(prev != null)
                return prev
            } else {
                return elem as E
            }
        }
    }

    fun put(index: Int, new: E) {
        require(index < size)
        var c = this
        while (true) {
            val elem = c.array[index].value
            if (elem is Moved<*>) {
                c = c.next.value!!
            } else if (c.array[index].compareAndSet(elem, new)) {
                return
            }
        }
    }

    fun compareAndSet(index: Int, expect: Any?, update: Any?): Boolean {
        require(index < size)
        return array[index].compareAndSet(expect, update)
    }
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME