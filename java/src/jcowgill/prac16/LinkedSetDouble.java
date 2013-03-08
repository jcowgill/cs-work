package jcowgill.prac16;

/**
 * An implementation of AbstractSetDouble using a linked list
 */
public class LinkedSetDouble extends AbstractSetDouble
{
	private NodeDouble head;
	private int size;

	/**
	 * Creates an empty LinkedSetDouble
	 */
	public LinkedSetDouble()
	{
	}

	@Override
	public boolean add(Double d)
	{
	    // Verify this double does not exist in the list
	    if (contains(d))
	        return false;

	    // Add to the start
	    this.head = new NodeDouble(d, this.head);
	    this.size++;
	    return true;
	}

	@Override
	public boolean remove(Double d)
	{
        // Scan the list for d
	    NodeDouble previous = null;
        NodeDouble current = this.head;

        while (current != null)
        {
            if (current.getValue().equals(d))
            {
                // Replace previous.next with current.next
                if (previous == null)
                    this.head = current;
                else
                    previous.setNext(current);

                this.size--;
                return true;
            }

            previous = current;
            current = current.getNext();
        }

        return false;
	}

	@Override
	public void clear()
	{
	    this.head = null;
	    this.size = 0;
	}

	@Override
	public boolean contains(Double d)
	{
        // Scan the list for d
        NodeDouble current = this.head;

        while (current != null)
        {
            if (current.getValue().equals(d))
                return true;

            current = current.getNext();
        }

        return false;
	}

	@Override
	public int size()
	{
		return size;
	}
}
