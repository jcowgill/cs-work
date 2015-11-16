package jcowgill.embs1;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.data.IntToken;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

public class SourceNode extends TypedAtomicActor
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
	private final SourceController controller = new SourceController(CHANNELS);

	/** Initializes a new SourceNode */
	public SourceNode(CompositeEntity container, String name)
			throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		// Setup ports
		input = new TypedIOPort(this, "input", true, false);
		output = new TypedIOPort(this, "output", false, true);
		setChannel = new TypedIOPort(this, "setChannel", false, true);

		input.setTypeEquals(BaseType.INT);
		output.setTypeEquals(BaseType.INT);
		setChannel.setTypeEquals(BaseType.INT);
	}

	/**
	 * Sets a new channel to send / receive on
	 *
	 * @param channel channel number to use (starts from 0)
	 */
	private void setChannel(int channel) throws IllegalActionException
	{
		setChannel.send(0, new IntToken(EXT_CHANNEL_OFFSET + channel));
	}

	@Override
	public void initialize() throws IllegalActionException
	{
		// Reset the controller and set the initial channel
		controller.reset();
		setChannel(controller.getReadChannel());
	}

	@Override
	public void fire() throws IllegalActionException
	{
		long time = (long) (getDirector().getModelTime().getDoubleValue() * 1000);

		// Handle any received tokens
		//  We only accept the last token received
		int nValue = -1;

		while (input.hasNewToken(0))
			nValue = ((IntToken) input.get(0)).intValue();

		if (nValue >= 1)
			controller.receiveBeacon(time, nValue);

		// TODO The rest of this function
	}
}
