package queue;

public interface Queue {

    /*
        Model:
            [a0, a1, ..., a"n-1"]
            n - queue size
            head = a0 if n > 0 else undefined
            tail = a'n-1' if n > 0 else undefined

        Inv:
            n >= 0
            for all i = 0..n-1: a[i] != null
            LIFO - last in, first out
            Ability to add elements to the end of the queue
            Ability to remove element from the head of the queue

        Immutable:
            for all i = 0..n-1: a[i] == a'[i]

        Note:
            var' means just modified var

    */

    // pred: element != null
    // post: n' = n + 1, a'[n'-1] == element, for all i = 0..n-1: a[i] == a'[i]

    void enqueue(Object element);

    // pred: n > 0
    // post: value = a[0] != null, n' = n - 1, for all i = 1..n-1: a[i] == a'[i-1]

    Object dequeue();

    // pred: n > 0
    // post: value = a[n-1] != null, immutable

    Object element();

    // pred: element != null
    // post: value = (|{i: a[i] == element}| > 0), immutable

    boolean contains(Object element);

    // pred: element != null
    // post: k = min({i: a[i] == element} U {n+1})
    //       for all i = 1...k-1 and k+1...n: a'[i] = a[i]
    //       n' = k-1 + max(n-k, 0)
    //       value = (k != n+1)

    boolean removeFirstOccurrence(Object element);

    // post: value = size, immutable

    int size();

    // post: value = true if (n == 0) else false, immutable

    boolean isEmpty();

    // post: don't store any links to the objects anymore, n' = 0

    void clear();


}
