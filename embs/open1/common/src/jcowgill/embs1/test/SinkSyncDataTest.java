package jcowgill.embs1.test;

import static org.junit.Assert.*;
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
		sink.receiveBeacon(2, 0);
		assertThat(sink.calcReceptionPhase(0, true), is(0L));
		sink.receiveBeacon(1, 1000);
		assertThat(sink.calcReceptionPhase(0, true), not(0L));
	}

	/** Test a "nice" sink without a good n value */
	@Test
	public void testNiceNoN()
	{
		sink.receiveBeacon(2, 0);
		sink.receiveBeacon(1, 1000);

		assertThat(sink.hasGoodDeltaT(), is(true));
		assertThat(sink.calcReceptionPhase(1100), is(2000L));
		assertThat(sink.calcReceptionPhase(2000), is(2000L));
		assertThat(sink.calcReceptionPhase(2100), is(2100L));
		assertThat(sink.calcReceptionPhase(2500), is(0L));
		assertThat(sink.calcReceptionPhase(2600), is(0L));
	}

	/** Test a "nice" sink with a good n value */
	@Test
	public void testNiceWithN()
	{
		sink.receiveBeacon(2, 0);
		sink.receiveBeacon(1, 1000);
		sink.receiveBeacon(2, 13000);

		assertThat(sink.hasGoodDeltaT(), is(true));
		assertThat(sink.calcReceptionPhase(13100), is(15000L));
		assertThat(sink.calcReceptionPhase(16000), is(28000L));
	}

	/** Test a n=1 sink with no packet loss */
	@Test
	public void testNIs1()
	{
		sink.receiveBeacon(1, 0);
		sink.receiveBeacon(1, 12000);

		assertThat(sink.hasGoodDeltaT(), is(false));
		assertThat(sink.calcReceptionPhase(12100), is(13000L));
		assertThat(sink.calcReceptionPhase(24000), is(0L));
		assertThat(sink.calcReceptionPhase(24000, true), is(25000L));
	}

	/** Test a n=1 sink with some packet loss */
	@Test
	public void testNIs1Lossy()
	{
		sink.receiveBeacon(1, 0);
		sink.receiveBeacon(1, 12000);
	}
}
