package jcowgill.prac17;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * A doubly linked list implementation
 */
public class LinkedList<T> implements Stack<T>, Deque<T>, Iterable<T>
{
    private final Node<T> sentinel = new Node<T>(null);
    private int size;

    @Override
    public void pushFront(T d)
    {
        // Create new node and insert after sentinel
        new Node<T>(d).insertAfter(sentinel);
        size++;
    }

    @Override
    public void pushBack(T d)
    {
        // Create new node and before after sentinel
        new Node<T>(d).insertBefore(sentinel);
        size++;
    }

    /**
     * Removes the given node and returns its value
     *
     * @param node node to remove
     * @return the node's value
     * @throws NoSuchElementException if node is the sentinel
     */
    private T popNode(Node<T> node)
    {
        // Check for sentinel
        if (node == sentinel)
            throw new NoSuchElementException();

        // Remove and return value
        node.remove();
        size--;
        return node.value;
    }

    @Override
    public T popFront()
    {
        return popNode(sentinel.next);
    }

    @Override
    public T popBack()
    {
        return popNode(sentinel.prev);
    }

    /**
     * Gets the value of the given node and returns its value
     *
     * @param node node to peek
     * @return the node's value
     * @throws NoSuchElementException if node is the sentinel
     */
    private T peekNode(Node<T> node)
    {
        // Check for sentinel
        if (node == sentinel)
            throw new NoSuchElementException();

        // Return value
        return node.value;
    }

    @Override
    public T peekFront()
    {
        return peekNode(sentinel.next);
    }

    @Override
    public T peekBack()
    {
        return peekNode(sentinel.prev);
    }

    @Override
    public T peek()
    {
        return peekBack();
    }

    @Override
    public T pop()
    {
        return popBack();
    }

    @Override
    public void push(T d)
    {
        pushBack(d);
    }

    @Override
    public int size()
    {
        return size;
    }

    @Override
    public boolean isEmpty()
    {
        return size == 0;
    }

    @Override
    public Iterator<T> iterator()
    {
        return new Iterator<T>()
        {
            private Node<T> last = sentinel;

            @Override
            public boolean hasNext()
            {
                return last.next != sentinel;
            }

            @Override
            public T next()
            {
                // Done?
                if (!hasNext())
                    throw new NoSuchElementException();

                // Advance pointer + return its value
                last = last.next;
                return last.value;
            }

            @Override
            public void remove()
            {
                // Remove current value + move to previous
                if (last == sentinel)
                    throw new IllegalStateException();

                Node<T> previous = last.prev;
                last.remove();
                last = previous;
            }
        };
    }

    /**
     * Class used for storing node values
     */
    private static class Node<T>
    {
        public final T value;
        public Node<T> prev, next;

        /**
         * Creates a new node
         * @param value value to store in the node
         */
        public Node(T value)
        {
            this.value = value;
        }

        /**
         * Inserts this node before the given node
         * @param other other node
         */
        public void insertBefore(Node<T> other)
        {
            assert prev == null && next == null;

            // Adjust pointers
            this.prev = other.prev;
            this.next = other;
            other.prev.next = this;
            other.prev = this;
        }

        /**
         * Inserts this node after the given node
         * @param other other node
         */
        public void insertAfter(Node<T> other)
        {
            assert prev == null && next == null;

            // Adjust pointers
            this.next = other.next;
            this.prev = other;
            other.next.prev = this;
            other.next = this;
        }

        /**
         * Removes this node from the list
         */
        public void remove()
        {
            assert prev != null && next != null;
            assert prev != next;

            // Adjust pointers
            this.next.prev = this.prev;
            this.prev.next = this.next;
            this.next = null;
            this.prev = null;
        }
    }
}
