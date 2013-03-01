package jcowgill.prac15;

/**
 * Exception thrown if an unknown city is used
 */
public class UnknownCityException extends RuntimeException
{
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new UnknownCityException with the given message
	 *
	 * @param msg exception message
	 */
	public UnknownCityException(String msg)
	{
		super(msg);
	}
}
