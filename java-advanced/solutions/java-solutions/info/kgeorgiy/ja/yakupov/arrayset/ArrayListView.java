package info.kgeorgiy.ja.yakupov.arrayset;

import java.util.*;

public class ArrayListView<T> extends AbstractList<T> implements RandomAccess {
    private final List<T> list;
    private final int head, tail;
    private final boolean reversed;

    public ArrayListView() {
        this(Collections.emptyList(), 0, 0, false);
    }

    // arraylist should not be modified outside
    // :NOTE: any list
    public ArrayListView(ArrayList<T> list) {
        this.list = Collections.unmodifiableList(list);
        head = 0;
        tail = list.size();
        reversed = false;
    }

    private ArrayListView(List<T> list, int head, int tail, boolean reversed) {
        this.list = list;
        this.head = head;
        this.tail = tail;
        this.reversed = reversed;
    }

    @Override
    public T get(int index) {
        return list.get(reversed ? tail - 1 - index : head + index);
    }

    @Override
    public int size() {
        return tail - head;
    }

    public ArrayListView<T> subList(int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex > size()) {
            throw new IndexOutOfBoundsException("fromIndex < 0 || toIndex > size()");
        }
        if (fromIndex > toIndex) {
            throw new IllegalArgumentException("fromIndex > toIndex");
        }
        if (reversed) {
            return new ArrayListView<>(list, tail - toIndex, tail - 1 - fromIndex + 1, true);
        }
        return new ArrayListView<>(list, head + fromIndex, head + toIndex, false);
    }

    public ArrayListView<T> reverse() {
        return new ArrayListView<>(list, head, tail, !reversed);
    }
}
