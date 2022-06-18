package queue;

public class ArrayQueue extends AbstractQueue {

    private int head;
    private Object[] elements = new Object[10];

    @Override
    protected void enqueueImpl(Object element) {
        ensureCapacity(size + 1);
        elements[getTail(0)] = element;
    }

    @Override
    protected void dequeueImpl() {
        elements[head] = null;
        head = (head + 1) % elements.length;
    }

    @Override
    protected Object elementImpl() {
        return elements[head];
    }

    @Override
    protected AbstractQueue createBlankQueue() {
        return new ArrayQueue();
    }

    @Override
    protected void applyInstance(AbstractQueue q) {
        ArrayQueue e = (ArrayQueue) q;
        head = e.head;
        elements = e.elements;
    }

    // post: immutable

    private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            Object[] tmp = new Object[elements.length * 2];
            int tail = getTail(-1);

            if (head <= tail) {
                System.arraycopy(elements, head, tmp, 0, size);
            } else {
                System.arraycopy(elements, head, tmp, 0, elements.length - head);
                System.arraycopy(elements, 0, tmp, elements.length - head, tail + 1);
            }

            elements = tmp;
            head = 0;
        }
    }

    // post: immutable

    private int getTail(int delta) {
        return (head + size + delta) % elements.length;
    }


    // DEQUE MODIFICATION

    // pred: element != null
    // post: n' = n + 1, a'[0] == element, for all i = 0..n-1: a[i] == a'[i + 1]

    public void push(Object element) {
        assert element != null;

        ensureCapacity(size + 1);

        elements[(head - 1 + elements.length) % elements.length] = element;
        head = (head - 1 + elements.length) % elements.length;
        size++;
    }

    // pred: n > 0
    // post: value = a[n-1], n' = n - 1, for all i = 0..n-2: a[i] == a'[i]

    public Object remove() {
        assert size > 0;

        Object value = peek();

        elements[(head + size - 1 + elements.length) % elements.length] = null;
        size--;

        return value;
    }

    // pred: n > 0
    // post: value = a[n-1], immutable

    public Object peek() {
        assert size > 0;

        return elements[(head + size - 1 + elements.length) % elements.length];
    }
}
