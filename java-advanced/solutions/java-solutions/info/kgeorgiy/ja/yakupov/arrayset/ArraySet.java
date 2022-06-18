package info.kgeorgiy.ja.yakupov.arrayset;

import java.util.*;

public class ArraySet<T> extends AbstractSet<T> implements NavigableSet<T> {
    private final ArrayListView<T> view;
    private final Comparator<? super T> comparator;

    public ArraySet() {
        this(new ArrayListView<>(), null);
    }

    public ArraySet(Collection<? extends T> other) {
        this(other, null);
    }

    public ArraySet(Comparator<? super T> cmp) {
        this(new ArrayListView<>(), cmp);
    }

    public ArraySet(Collection<? extends T> other, Comparator<? super T> cmp) {
        // :NOTE: Set
        TreeSet<T> tmp = new TreeSet<>(cmp);
        tmp.addAll(Objects.requireNonNull(other));
        // :NOTE: List.copyOf
        view = new ArrayListView<>(new ArrayList<>(tmp));
        comparator = cmp;
    }

    private ArraySet(ArrayListView<T> other, Comparator<? super T> cmp) {
        view = other;
        comparator = cmp;
    }

    // UTILITIES

    private int binarySearch(T e) {
        // :NOTE: procesing nulls
        return Collections.binarySearch(view, Objects.requireNonNull(e), comparator);
    }

    private int findLowerIndex(T e, boolean lower) {
        int i = binarySearch(e);
        if (i < 0) {
            return -(i + 1) - 1;
        }
        return lower ? i - 1 : i;
    }

    private int findUpperIndex(T e, boolean higher) {
        return findLowerIndex(e, !higher) + 1;
    }

    private T get(int index) {
        if (0 <= index && index < size()) {
            return view.get(index);
        }
        return null;
    }

    private T getOrThrow(int index) {
        T obj = get(index);
        if (obj == null) {
            throw new NoSuchElementException();
        }
        return obj;
    }

    @SuppressWarnings("unchecked")
    private int compare(T a, T b) {
        if (comparator != null) {
            return comparator.compare(a, b);
        }
        return ((Comparable<T>) a).compareTo(b);
    }

    // ABSTRACT COLLECTION

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return binarySearch((T) Objects.requireNonNull(o)) >= 0;
    }

    // clear is not supported -> throw exception

    @Override
    public Iterator<T> iterator() {
        return view.iterator();
    }

    @Override
    public int size() {
        return view.size();
    }

    // NAVIGABLE SET INTERFACE

    // complexities:
    // lower(), floor(), ceiling(), higher() - O(logN)
    // descendingSet(), descendingIterator() - O(1)
    // subSet(), headSet(), tailSet() - O(logN)

    @Override
    public T lower(T e) {
        return get(findLowerIndex(e, true));
    }

    @Override
    public T floor(T e) {
        return get(findLowerIndex(e, false));
    }

    @Override
    public T ceiling(T e) {
        return get(findUpperIndex(e, false));
    }

    @Override
    public T higher(T e) {
        return get(findUpperIndex(e, true));
    }

    @Override
    public T pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public NavigableSet<T> descendingSet() {
        return new ArraySet<>(view.reverse(), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<T> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<T> subSet(T fromElement, boolean fromInclusive, T toElement, boolean toInclusive) {
        if (compare(fromElement, toElement) <= 0) {
            return headSet(toElement, toInclusive).tailSet(fromElement, fromInclusive);
        }
        throw new IllegalArgumentException("fromElement is greater than toElement");
    }

    @Override
    public NavigableSet<T> headSet(T toElement, boolean inclusive) {
        int ind = findUpperIndex(toElement, inclusive);
        return new ArraySet<>(view.subList(0, ind), comparator);
    }

    @Override
    public NavigableSet<T> tailSet(T fromElement, boolean inclusive) {
        int ind = findUpperIndex(fromElement, !inclusive);
        return new ArraySet<>(view.subList(ind, view.size()), comparator);
    }

    @Override
    public SortedSet<T> subSet(T fromElement, T toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return tailSet(fromElement, true);
    }

    // SORTED SET INTERFACE

    // complexities:
    // first(), last() - O(1)

    @Override
    public Comparator<? super T> comparator() {
        return comparator;
    }

    @Override
    public T first() {
        return getOrThrow(0);
    }

    @Override
    public T last() {
        return getOrThrow(size() - 1);
    }
}
