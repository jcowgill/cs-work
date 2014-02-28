import java.util.Arrays;
import java.util.concurrent.CyclicBarrier;

/**
 * Good array filling
 */
public class GoodArrayFill
{
	/**
	 * When run, fills an array with a value
	 */
	private static class Filler implements Runnable
	{
		private static CyclicBarrier barrier = new CyclicBarrier(2);

		private final int[] array;
		private final int value;
		private final boolean oddIndexesFirst;

		public Filler(int[] array, int value, boolean oddIndexesFirst)
		{
			this.array = array;
			this.value = value;
			this.oddIndexesFirst = oddIndexesFirst;
		}

		@Override
		public void run()
		{
			// Fill array starting at offset and incrementing by 2 each time
			int offset1 = oddIndexesFirst ? 1 : 0;
			for (int i = offset1; i < array.length; i += 2)
				array[i] = value;

			try
			{
				// Synchronize
				barrier.await();
			}
			catch (Exception e)
			{
				throw new RuntimeException(e);
			}

			// Fill array starting at the other offset
			int offset2 = 1 - offset1;
			for (int i = offset2; i < array.length; i += 2)
				array[i] = value;
		}
	}

	public static void main(String[] args) throws InterruptedException
	{
		// Create array and threads
		int[] array = new int[10];
		Thread t1 = new Thread(new Filler(array, 1, true));
		Thread t2 = new Thread(new Filler(array, 7, false));

		// Fill array
		t1.start();
		t2.start();
		t1.join();
		t2.join();

		// Print result
		System.out.println(Arrays.toString(array));
	}
}
