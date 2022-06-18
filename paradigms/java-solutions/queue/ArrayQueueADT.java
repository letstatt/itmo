package queue;

import java.util.Arrays;

public class ArrayQueueADT {

    /*
        Model:
            [a0, a1, ..., a"n-1"]
            n - queue (modified - deque) size

        Inv:
            n >= 0
            for all i = 0..n-1: a[i] != null
            LIFO - last in, first out (for queue only)
            Ability to add elements to the end of the queue
            Ability to take or remove elements from the head of the queue
            Additional ability to add to the head and take and remove from the end (for deque)

        Immutable:
            for all i = 0..n-1: a[i] == a'[i]

        Note:
            var' means just modified var

    */


    private int size, head;
    private Object[] elements = new Object[10];

    // pred: instance != null, element != null
    // post: n' = n + 1, a'[n'-1] == element, for all i = 0..n-1: a[i] == a'[i]

    public static void enqueue(ArrayQueueADT instance, Object element) {
        assert element != null;

        ensureCapacity(instance, instance.size + 1);

        instance.elements[(instance.head + instance.size) % instance.elements.length] = element;
        instance.size++;
    }

    // pred: instance != null, element != null
    // post: n' = n + 1, a'[0] == element, for all i = 0..n-1: a[i] == a'[i + 1]

    public static void push(ArrayQueueADT instance, Object element) {
        assert element != null;

        ensureCapacity(instance, instance.size + 1);

        instance.elements[(instance.head - 1 + instance.elements.length) % instance.elements.length] = element;
        instance.head = (instance.head - 1 + instance.elements.length) % instance.elements.length;
        instance.size++;
    }

    // pred: instance != null, n > 0
    // post: value = a[0] != null, n' = n - 1, for all i = 1..n-1: a[i] == a'[i-1]

    public static Object dequeue(ArrayQueueADT instance) {
        assert instance.size > 0;

        Object value = element(instance);

        instance.elements[instance.head] = null;
        instance.head = (instance.head + 1) % instance.elements.length;
        instance.size--;

        return value;
    }

    // pred: instance != null, n > 0
    // post: value = a[n-1] != null, immutable

    public static Object element(ArrayQueueADT instance) {
        assert instance.size > 0;

        return instance.elements[instance.head];
    }

    // pred: instance != null, n > 0
    // post: value = a[n-1], n' = n - 1, for all i = 0..n-2: a[i] == a'[i]

    public static Object remove(ArrayQueueADT instance) {
        assert instance.size > 0;

        Object value = peek(instance);

        instance.elements[(instance.head + instance.size - 1 +
                instance.elements.length) % instance.elements.length] = null;
        instance.size--;

        return value;
    }

    // pred: instance != null, n > 0
    // post: value = a[n-1], immutable

    public static Object peek(ArrayQueueADT instance) {
        assert instance.size > 0;

        return instance.elements[(instance.head + instance.size - 1 +
                instance.elements.length) % instance.elements.length];
    }

    // pred: instance != null
    // post: value = size, immutable

    public static int size(ArrayQueueADT instance) {
        return instance.size;
    }

    // pred: instance != null
    // post: value = true if (n == 0) else false, immutable

    public static boolean isEmpty(ArrayQueueADT instance) {
        return instance.size == 0;
    }

    // pred: instance != null
    // post: don't store any links to the objects anymore, n' = 0

    public static void clear(ArrayQueueADT instance) {
        if (instance.size > 0) {
            instance.elements = new Object[10];
            instance.head = 0;
            instance.size = 0;
        }
    }

    // pred: instance != null
    // post: immutable

    private static void ensureCapacity(ArrayQueueADT instance, int capacity) {
        if (capacity > instance.elements.length) {
            Object[] tmp = new Object[instance.elements.length * 2];
            int tail = (instance.head + instance.size - 1) % instance.elements.length;

            if (instance.head <= tail) {
                System.arraycopy(instance.elements, instance.head, tmp, 0, instance.size);
            } else {
                System.arraycopy(instance.elements, instance.head, tmp,
                        0, instance.elements.length - instance.head);
                System.arraycopy(instance.elements, 0, tmp,
                        instance.elements.length - instance.head, tail + 1);
            }

            instance.elements = tmp;
            instance.head = 0;
        }
    }
}
