/**
 * A barrier which does not release any threads until all have arrived
 */
public class Barrier
{
	private final Object syncLock = new Object();
	private final int totalThreads;

	private int waitingThreads;
	private int version;

	/**
	 * Initializes the barrier
	 *
	 * @param threads number of threads which need to arrive to release the barrier
	 */
	public Barrier(int threads)
	{
		this.totalThreads = threads;
	}

	/**
	 * Waits until all the threads are waiting before releasing all of them
	 *
	 * <p>
	 * There is a race condition involving {@link InterruptedException}. You cannot assume that
	 * the threads have not been released yet when that exception is thrown.
	 */
	public void await() throws InterruptedException
	{
		synchronized (syncLock)
		{
			int myVersion = version;

			// Release if we have enough threads
			if (waitingThreads + 1 >= totalThreads)
			{
				version++;
				waitingThreads = 0;
				syncLock.notifyAll();
			}
			else
			{
				// Add myself to waiting threads and wait until version is changed
				waitingThreads++;

				try
				{
					while (myVersion == version)
						syncLock.wait();
				}
				catch (InterruptedException e)
				{
					// Not waiting anymore
					if (myVersion == version)
						waitingThreads--;

					throw e;
				}
			}
		}
	}
}
