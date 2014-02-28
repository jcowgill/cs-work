/**
 * Bad multithreaded increment
 */
public class BadIncrement
{
	private static class Pair
	{
		public int x, y;

		public void increment()
		{
			this.x++;
			this.y++;
		}

		@Override
		public String toString()
		{
			return "Pair{x=" + x + ", y=" + y + '}';
		}
	}

	private static class IncrementThread implements Runnable
	{
		private Pair pair;

		public IncrementThread(Pair pair)
		{
			this.pair = pair;
		}

		@Override
		public void run()
		{
			this.pair.increment();
		}
	}

	public static void main(String[] args) throws InterruptedException
	{
		// Create objects
		Pair pair = new Pair();
		Thread t1 = new Thread(new IncrementThread(pair));
		Thread t2 = new Thread(new IncrementThread(pair));

		// Run threads and wait for them
		t1.start();
		t2.start();
		t1.join();
		t2.join();

		// Print result
		System.out.println(pair);
	}
}
