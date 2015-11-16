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
	/** The SinkSyncData objects for each sink */
	private final SinkSyncData[] sinkData;

	/**
	 * Current channel to listen for beacons on
	 *
	 * Channels are numbered from 0. If readChannel == -1, nothing is to be read.
	 */
	private int readChannel;

	/**
	 * Initializes a SourceController object
	 *
	 * @param channels the number of channels available
	 */
	public SourceController(int channels)
	{
		sinkData = new SinkSyncData[channels];

		for (int i = 0; i < channels; i++)
			sinkData[i] = new SinkSyncData();

		reset();
	}

	/** Returns the number of channels */
	public int getChannelCount()
	{
		return sinkData.length;
	}

	/** Returns the channel to read on (while not sending) */
	public int getReadChannel()
	{
		return readChannel;
	}

	/** Returns the next send event or null if there are no pending events */
	public SendEvent getNextSendEvent()
	{
		// TODO Implement this
		return null;
	}

	/** Resets the controller */
	public void reset()
	{
		// Reset all sinks
		for (int i = 0; i < getChannelCount(); i++)
			sinkData[i].reset();

		// Reset the read channel
		readChannel = 0;
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

		// TODO the rest of this function
	}

	/**
	 * Called after a send completed on the given channel
	 *
	 * @param channel channel the send completed on
	 */
	public void sendComplete(long absoluteTime, int channel)
	{
		// TODO do we need this if we have getNextSendEvent ?
	}

	/**
	 * Called periodically to perform any needed channel hopping
	 *
	 * @param absoluteTime the absolute time
	 */
	public void periodicTimer(long absoluteTime)
	{
		// TODO Implement this
	}
}
