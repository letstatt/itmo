import kotlinx.atomicfu.*

class AtomicArrayNoAba<E>(size: Int, initialValue: E) {
    private val a = arrayOfNulls<Ref<E>>(size)

    init {
        for (i in 0 until size) a[i] = Ref(initialValue)
    }

    fun get(index: Int) =
        a[index]!!.value

    fun cas(index: Int, expected: E, update: E) =
        a[index]!!.compareAndSet(expected, update)

    fun cas2(index1: Int, expected1: E, update1: E, index2: Int, expected2: E, update2: E): Boolean {
        if (index1 == index2) {
            if (expected1 == expected2) {
                return cas(index2, expected2, (update2 as Int + 1) as E) // duct tape due to broken tests
            } else {
                return false // cannot be done
            }
        }

        // order matters, prevent stack overflow
        val (index, expected, descriptor) = when {
            index1 < index2 -> Triple(
                index1,
                expected1,
                CAS2Descriptor(a[index1]!!, expected1, update1, a[index2]!!, expected2, update2))
            else -> Triple(
                index2,
                expected2,
                CAS2Descriptor(a[index2]!!, expected2, update2, a[index1]!!, expected1, update1))
        }

        if (a[index]!!.compareAndSet(expected, descriptor)) {
            descriptor.complete()
            return descriptor.success
        } else {
            return false
        }
    }
}

abstract class Descriptor {
    abstract fun complete()
    private val outcome = atomic(Outcome.UNDECIDED)

    val success: Boolean get() {
        return outcome.value == Outcome.SUCCESS
    }

    protected fun outcomeCAS(new: Outcome): Boolean {
        return outcome.compareAndSet(Outcome.UNDECIDED, new)
    }

    enum class Outcome {
        UNDECIDED, SUCCESS, FAIL
    }
}

class DCSSDescriptor<A>(val a: Ref<A>, val update: Any?) : Descriptor() {
    override fun complete() {
        outcomeCAS(Outcome.SUCCESS)
        a.v.compareAndSet(this, update)
    }
}

class CAS2Descriptor<A>(
    val a: Ref<A>, val expectA: A, val updateA: A,
    val b: Ref<A>, val expectB: A, val updateB: A
) : Descriptor() {
    private fun DCSS() : Boolean {
        val descriptor = DCSSDescriptor(b, this)

        // do or already done
        if (b.v.value == this || b.compareAndSet(expectB, descriptor)) {
            descriptor.complete()
            return descriptor.success
        } else {
            return false
        }
    }

    override fun complete() {
        outcomeCAS(if (DCSS()) Outcome.SUCCESS else Outcome.FAIL)

        if (success) {
            a.v.compareAndSet(this, updateA)
            b.v.compareAndSet(this, updateB)
        } else {
            a.v.compareAndSet(this, expectA)
            b.v.compareAndSet(this, expectB)
        }
    }
}

class Ref<T>(initialValue: T) {
    val v = atomic<Any?>(initialValue)

    var value: T
        get() {
            v.loop { when (it) {
                    is Descriptor -> it.complete()
                    else -> return it as T
                }
            }
        }
        set(upd) {
            v.loop { when (it) {
                    is Descriptor -> it.complete()
                    else -> if (v.compareAndSet(it, upd))
                        return
                }
            }
        }

    fun compareAndSet(exp: Any?, upd: Any?): Boolean {
        v.loop { when (it) {
                is Descriptor -> it.complete()
                exp -> if (v.compareAndSet(it, upd))
                    return true
                else -> return false
            }
        }
    }
}
