package jcowgill.embs1;

import ptolemy.actor.Director;
import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.actor.util.Time;
import ptolemy.data.IntToken;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

public class PtolemySourceNode extends TypedAtomicActor
{
	/** Number of channels */
	private static final int CHANNELS = 5;

	/** Number added to internal channels to get the external channel number */
	private static final int EXT_CHANNEL_OFFSET = 11;

	/** Input and output data ports */
	private final TypedIOPort input, output;

	/** Port to change the wireless channel */
	private final TypedIOPort setChannel;

	/** The controller for the source node */
	private final SourceController controller = new SourceController(CHANNELS, 0);

	/** The channel we're currently on */
	private int currentChannel;

	/** The current scheduled wakeup time */
	private Time timerTime;

	/** True if the next fire event is a packet sending wakeup */
	private boolean isSending;

	/** Initializes a new PtolemySourceNode */
	public PtolemySourceNode(CompositeEntity container, String name)
			throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		// Setup ports
		input = new TypedIOPort(this, "input", true, false);
		setChannel = new TypedIOPort(this, "setChannel", false, true);
		output = new TypedIOPort(this, "output", false, true);

		input.setTypeEquals(BaseType.INT);
		setChannel.setTypeEquals(BaseType.INT);
		output.setTypeEquals(BaseType.INT);
	}

	/**
	 * Sets a new channel to send / receive on
	 *
	 * @param channel channel number to use (starts from 0)
	 */
	private void setChannel(int channel) throws IllegalActionException
	{
		// We can't read "no channel", so use channel 0 in that case
		if (channel < 0)
			channel = 0;

		if (currentChannel != channel)
		{
			setChannel.send(0, new IntToken(EXT_CHANNEL_OFFSET + channel));
			currentChannel = channel;
		}
	}

	/**
	 * Sets the next time the node will wakeup for a timer event,
	 * cancelling any previous event
	 *
	 * @param time the time to wakeup
	 */
	private void setWakeupTime(Time time) throws IllegalActionException
	{
		// Schedule next timer event
		timerTime = time;
		if (time != null)
			getDirector().fireAt(this, time);
	}

	/** Updates the state of the source for reading  */
	private void setReading() throws IllegalActionException
	{
		// Set read channel and schedule next wakeup
		setChannel(controller.getReadChannel());
		setWakeupTime(new Time(getDirector(), ((double) controller.getNextWakeupTime()) / 1000));
	}

	/**
	 * Send at most one pending packet
	 *
	 * @return true if something was sent
	 */
	private boolean sendPendingPacket() throws IllegalActionException
	{
		// Send any packets if necessary
		int sendChannel = controller.calcSendChannel();
		if (sendChannel >= 0)
		{
			setChannel(sendChannel);
			output.send(0, new IntToken(sendChannel));

			// Fire immediately
			setWakeupTime(getDirector().getModelTime());
			isSending = true;
			return true;
		}

		return false;
	}

	@Override
	public void initialize() throws IllegalActionException
	{
		// Reset the controller and start reading
		controller.reset(0);
		currentChannel = -1;
		timerTime = null;
		isSending = false;

		setReading();
	}

	@Override
	public void fire() throws IllegalActionException
	{
		Director director = getDirector();
		long time = (long) Math.ceil(director.getModelTime().getDoubleValue() * 1000);

		// Handle any received tokens
		//  We only accept the last token received
		int nValue = -1;

		while (input.hasToken(0))
			nValue = ((IntToken) input.get(0)).intValue();

		if (nValue >= 1)
		{
			controller.receiveBeacon(time, nValue);
		}
		else
		{
			// Ensure this is an intended timer event
			if (timerTime != null && !director.getModelTime().equals(timerTime))
				return;
		}

		// Tell the controller that we've woken up
		//  To ensure we don't get any sending loops, we do not do this if this
		//  was a sending wakeup
		if (!isSending)
			controller.wakeupEvent(time);

		isSending = false;

		// Send any packets if necessary
		if (!sendPendingPacket())
		{
			// If nothing was sent, enter read mode
			setReading();
		}
	}
}
