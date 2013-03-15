package jcowgill.prac17;

import java.util.NoSuchElementException;

/**
 * Interface for deques containing doubles
 */
public interface Deque<T>
{
    /**
     * Pushes a new item at the front of the deque
     *
     * @param d item to push
     */
    public void pushFront(T d);

    /**
     * Pushes a new item at the back of the deque
     *
     * @param d item to push
     */
    public void pushBack(T d);

    /**
     * Pops an item from the front of the deque
     *
     * @return item at the front of the deque
     * @throws NoSuchElementException thrown if the deque is empty
     */
    public T popFront();

    /**
     * Pops an item from the back of the deque
     *
     * @return item at the back of the deque
     * @throws NoSuchElementException thrown if the deque is empty
     */
    public T popBack();

    /**
     * Peeks the item at the front of the deque
     *
     * @return item at the front of the deque
     * @throws NoSuchElementException thrown if the deque is empty
     */
    public T peekFront();

    /**
     * Peeks the item at the back of the deque
     *
     * @return item at the back of the deque
     * @throws NoSuchElementException thrown if the deque is empty
     */
    public T peekBack();

    /**
     * Returns the size of the deque
     *
     * @return the size of the deque
     */
    public int size();

    /**
     * Returns true if the deque is empty
     *
     * @return true if the deque is empty
     */
    public boolean isEmpty();
}
