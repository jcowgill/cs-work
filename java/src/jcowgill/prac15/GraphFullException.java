package jcowgill.prac15;

/**
 * Exception thrown if the graph is full
 */
public class GraphFullException extends RuntimeException
{
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new GraphFullException with the given message
	 *
	 * @param msg exception message
	 */
	public GraphFullException(String msg)
	{
		super(msg);
	}
}
