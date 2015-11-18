package jcowgill.embs1;

/**
 * The main controller for a source node
 *
 * Among other things, this class controls the "math" side of:
 * - Owning the SinkSyncData objects for multiple sinks
 * - Channel hopping (which channels to receive beacons on)
 * - When to send packets
 */
public class SourceController
{
	/** Default time before hopping to another channel */
	private static final long TIME_HOP = 1000;

	/** Extra time to give a channel if data is received on it */
	private static final long TIME_HOP_DATA = 1550;

	/** The SinkSyncData objects for each sink */
	private final SinkSyncData[] sinkData;

	/**
	 * If 1 for a channel, a packet should be sent asap
	 *
	 * This is a byte array since moterunner does not support boolean arrays.
	 */
	private final byte[] sendPending;

	/** The next time the source should wakeup for a send event. This is -1 if nothing is pending. */
	private long nextSendWakeup;

	/**
	 * Current channel to listen for beacons on
	 *
	 * Channels are numbered from 0. If readChannel == -1, nothing is to be read.
	 */
	private int readChannel;

	/** Expiry time for the current channel hop. */
	private long hopExpiry;

	/** If true, the hop time has already been extended. */
	private boolean hopExtended;

	/**
	 * Initializes a SourceController object
	 *
	 * @param channels the number of channels available
	 * @param initialTime the initial absolute time
	 */
	public SourceController(int channels, long initialTime)
	{
		sinkData = new SinkSyncData[channels];
		sendPending = new byte[channels];

		for (int i = 0; i < channels; i++)
			sinkData[i] = new SinkSyncData();

		reset(initialTime);
	}

	/** Returns the number of channels */
	public int getChannelCount()
	{
		return sinkData.length;
	}

	/**
	 * Returns the channel to read on (while not sending)
	 *
	 * Returns -1 if no channel should be read.
	 * */
	public int getReadChannel()
	{
		return readChannel;
	}

	/**
	 * Returns the channel to send on immediately, or -1 if we shouldn't send now
	 *
	 * This method uses the last recorded time to base sending on, so you should
	 * only call this after calling one of {@link #receiveBeacon(long, int)} or
	 * {@link #wakeupEvent(long)} to indicate the time.
	 *
	 * After calling this method, the channel returned is marked as sent and won't
	 * be returned again (until sufficient time has passed).
	 *
	 * @return the channel to send on
	 */
	public int calcSendChannel()
	{
		for (int i = 0; i < getChannelCount(); i++)
		{
			if (sendPending[i] != 0)
			{
				sendPending[i] = 0;
				return i;
			}
		}

		return -1;
	}

	/** Returns the absolute time the next timer event should fire */
	public long getNextWakeupTime()
	{
		long lowest = nextSendWakeup;

		if (lowest == -1 || hopExpiry < lowest)
			lowest = hopExpiry;

		return lowest;
	}

	/**
	 * Resets the controller
	 *
	 * @param initialTime the initial absolute time
	 */
	public void reset(long initialTime)
	{
		// Reset all sinks
		for (int i = 0; i < getChannelCount(); i++)
			sinkData[i].reset();

		// Reset pending sends
		for (int i = 0; i < getChannelCount(); i++)
			sendPending[i] = 0;

		nextSendWakeup = -1;

		// Reset the read channel
		readChannel = 0;

		// Initialize hop timer
		hopExpiry = TIME_HOP;
		hopExtended = false;
	}

	/**
	 * Called when a beacon is received on the read channel
	 *
	 * @param n the value of n in the beacon received
	 * @param absoluteTime the absolute time the beacon was received in milliseconds
	 */
	public void receiveBeacon(long absoluteTime, int n)
	{
		// Ignore spurious receive
		if (readChannel < 0)
			return;

		// Forward to sink object
		sinkData[readChannel].receiveBeacon(absoluteTime, n);

		// If n is not 1, and this was the first beacon, extend the hop time
		//  to try and immediately get another beacon
		if (n != 1 && !hopExtended)
		{
			// Extend the hop if it was the first
			hopExpiry = absoluteTime + TIME_HOP_DATA;
			hopExtended = true;
		}
		else
		{
			// Otherwise there is no point staying on this channel at the moment
			changeChannel(absoluteTime);
		}
	}

	/**
	 * Select another channel to read on
	 */
	private void changeChannel(long absoluteTime)
	{
		int firstChannel = readChannel + 1;
		readChannel = -1;

		// Find another suitable channel
		for (int i = 0; i < getChannelCount(); i++)
		{
			int channel = (firstChannel + i) % getChannelCount();

			if (!sinkData[channel].hasGoodDeltaT() || !sinkData[channel].hasGoodN())
			{
				if (sinkData[channel].nextInterestingBeacon(absoluteTime) < absoluteTime + TIME_HOP)
				{
					// Suitable channel
					readChannel = channel;
					break;
				}
			}
		}

		// Reset timer
		hopExpiry = absoluteTime + TIME_HOP;
		hopExtended = false;
	}

	/**
	 * Called periodically to perform any needed channel hopping
	 *
	 * @param absoluteTime the absolute time
	 */
	public void wakeupEvent(long absoluteTime)
	{
		nextSendWakeup = -1;

		// Recalculate all the pending packet sends
		for (int i = 0; i < getChannelCount(); i++)
		{
			sendPending[i] = 0;

			long sendTime = sinkData[i].calcReceptionPhase(absoluteTime);

			if (sendTime > 0)
			{
				if (sendTime <= absoluteTime)
					sendPending[i] = 1;
				else if (nextSendWakeup == -1 || sendTime < nextSendWakeup)
					nextSendWakeup = sendTime;
			}
		}

		// Ensure the timer doesn't ever stop
		//  This can happen if for all channels
		//   goodN and goodT are true so rx is off
		//   everything is pending
		if (nextSendWakeup == -1)
			nextSendWakeup = absoluteTime + 2000;

		// Change channel if the hop timer has expired
		if (absoluteTime >= hopExpiry)
			changeChannel(absoluteTime);
	}
}
