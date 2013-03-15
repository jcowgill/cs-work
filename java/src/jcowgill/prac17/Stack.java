package jcowgill.prac17;

import java.util.NoSuchElementException;

/**
 * Interface for stacks of objects
 */
public interface Stack<T>
{
    /**
     * Peaks the top value on the stack without popping it off
     *
     * @return the top value on the stack
     * @throws NoSuchElementException thrown if the stack is empty
     */
    public T peek();

    /**
     * Pops the top value off the stack
     *
     * @return the top value on the stack
     * @throws NoSuchElementException thrown if the stack is empty
     */
    public T pop();

    /**
     * Pushes a value onto the stack
     *
     * @param d value to push
     */
    public void push(T d);

    /**
     * Returns the size of the stack
     *
     * @return the size of the stack
     */
    public int size();

    /**
     * Returns true if the stack is empty
     *
     * @return true if the stack is empty
     */
    public boolean isEmpty();
}
