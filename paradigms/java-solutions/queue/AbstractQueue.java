package queue;

import java.util.Objects;

public abstract class AbstractQueue implements Queue {

    protected int size;

    protected abstract void enqueueImpl(Object element);
    protected abstract void dequeueImpl();
    protected abstract Object elementImpl();
    protected abstract AbstractQueue createBlankQueue();
    protected abstract void applyInstance(AbstractQueue q);

    @Override
    public void enqueue(Object element) {
        assert element != null;

        enqueueImpl(element);
        size += 1;
        size();
    }

    @Override
    public Object dequeue() {
        assert !isEmpty();

        Object ret = element();
        dequeueImpl();
        size -= 1;

        size();
        return ret;
    }

    @Override
    public Object element() {
        assert !isEmpty();

        return elementImpl();
    }

    @Override
    public boolean contains(Object element) {
        return search(element, false);
    }

    @Override
    public boolean removeFirstOccurrence(Object element) {
        return search(element, true);
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public void clear() {
        while (!isEmpty()) {
            dequeue();
        }
    }

    private boolean search(Object elementToFind, boolean removeFirstOccurrence) {
        AbstractQueue q = createBlankQueue();
        boolean found = false;

        while (!isEmpty()) {
            Object elem = dequeue();

            if (elem.equals(elementToFind) && !found) {
                if (!removeFirstOccurrence) {
                    q.enqueue(elem);
                }
                found = true;

            } else {
                q.enqueue(elem);
            }
        }

        applyInstance(q);
        size = q.size;
        return found;
    }
}
