package queue;

public class LinkedQueue extends AbstractQueue {

    private Node tail;
    private Node head;

    public LinkedQueue() {
        head = new Node();
        tail = new Node();
        head.setNext(tail);
    }

    @Override
    protected void enqueueImpl(Object element) {
        tail.setValue(element);
        tail = tail.setNext(new Node(null));
    }

    @Override
    protected void dequeueImpl() {
        head.setNext(head.getNext().getNext());
    }

    @Override
    protected Object elementImpl() {
        return head.getNext().getValue();
    }

    @Override
    protected AbstractQueue createBlankQueue() {
        return new LinkedQueue();
    }

    @Override
    protected void applyInstance(AbstractQueue q) {
        LinkedQueue e = (LinkedQueue) q;
        head = e.head;
        tail = e.tail;
    }

    private static class Node {
        private Node next = null;
        private Object value = null;

        public Node() {}

        public Node(Object val) {
            value = val;
        }

        private Object getValue() {
            return value;
        }

        private void setValue(Object val) {
            value = val;
        }

        private Node setNext(Node node) {
            next = node;
            return node;
        }

        private Node getNext() {
            return next;
        }
    }
}
