import java.util.Arrays;

/**
 * Bad array filling
 */
public class BadArrayFill
{
	/**
	 * When run, fills an array with a value
	 */
	private static class Filler implements Runnable
	{
		private final int[] array;
		private final int value;

		public Filler(int[] array, int value)
		{
			this.array = array;
			this.value = value;
		}

		@Override
		public void run()
		{
			Arrays.fill(array, value);
		}
	}

	public static void main(String[] args) throws InterruptedException
	{
		// Create array and threads
		int[] array = new int[10];
		Thread t1 = new Thread(new Filler(array, 1));
		Thread t2 = new Thread(new Filler(array, 7));

		// Fill array
		t1.start();
		t2.start();
		t1.join();
		t2.join();

		// Print result
		System.out.println(Arrays.toString(array));
	}
}
