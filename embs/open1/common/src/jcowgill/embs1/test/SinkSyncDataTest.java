package jcowgill.embs1.test;

import static org.junit.Assert.*;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

import static org.hamcrest.CoreMatchers.*;

import org.junit.Before;
import org.junit.Test;

import jcowgill.embs1.SinkSyncData;

/** Test cases for SinkSyncData */
public class SinkSyncDataTest
{
	private SinkSyncData sink = new SinkSyncData();

	@Before
	public void setUp()
	{
		sink.reset();
	}

	/** Ensure calcReceptionPhase returns 0 at correct times */
	@Test
	public void testNoData()
	{
		assertThat(sink.calcReceptionPhase(0, true), is(0L));
		sink.receiveBeacon(0, 2);
		assertThat(sink.calcReceptionPhase(0, true), is(0L));
		sink.receiveBeacon(1000, 1);
		assertThat(sink.calcReceptionPhase(0, true), not(0L));
	}

	/** Test a "nice" sink without a good n value */
	@Test
	public void testNiceNoN()
	{
		sink.receiveBeacon(0, 2);
		sink.receiveBeacon(1000, 1);

		assertThat(sink.hasGoodDeltaT(), is(true));
		assertThat(sink.calcReceptionPhase(1100), withinReceptionPhase(2000, 1000));
		assertThat(sink.calcReceptionPhase(2000), withinReceptionPhase(2000, 1000));
		assertThat(sink.calcReceptionPhase(2100), withinReceptionPhase(2000, 1000));
		assertThat(sink.calcReceptionPhase(3000), is(0L));
		assertThat(sink.calcReceptionPhase(3100), is(0L));
	}

	/** Test a "nice" sink with a good n value */
	@Test
	public void testNiceWithN()
	{
		sink.receiveBeacon(0, 2);
		sink.receiveBeacon(1000, 1);
		sink.receiveBeacon(13000, 2);

		assertThat(sink.hasGoodDeltaT(), is(true));
		assertThat(sink.getIterationLength(), is(13000L));
		assertThat(sink.calcReceptionPhase(13100), withinReceptionPhase(15000, 1000));
		assertThat(sink.calcReceptionPhase(16000), withinReceptionPhase(28000, 1000));
	}

	/** Test a n=1 sink with no packet loss */
	@Test
	public void testNIs1()
	{
		sink.receiveBeacon(0, 1);
		sink.receiveBeacon(12000, 1);

		assertThat(sink.hasGoodDeltaT(), is(false));
		assertThat(sink.calcReceptionPhase(12100), withinReceptionPhase(13000, 1000));
		assertThat(sink.calcReceptionPhase(24000), is(0L));
		assertThat(sink.calcReceptionPhase(24000, true), withinReceptionPhase(25000, 1000));
	}

	/** Test sink reset */
	@Test
	public void testSinkReset()
	{
		// No methods should give any meaningful results
		assertThat(sink.hasGoodDeltaT(), is(false));
		assertThat(sink.hasGoodN(), is(false));
		assertThat(sink.calcReceptionPhase(0), is(0L));
		assertThat(sink.getIterationLength(), is(0L));
	}

	/** Creates a matcher for a reception phase */
	private static Matcher<Long> withinReceptionPhase(final long startTime, final long deltaT)
	{
		return new BaseMatcher<Long>()
		{
			@Override
			public boolean matches(Object arg0)
			{
				return (long) arg0 >= startTime && (long) arg0 <= startTime + deltaT;
			}

			@Override
			public void describeTo(Description arg0)
			{
				arg0.appendText("within reception phase starting ")
					.appendValue(startTime)
					.appendText(" with Δt = ")
					.appendValue(deltaT);
			}
		};
	}
}
