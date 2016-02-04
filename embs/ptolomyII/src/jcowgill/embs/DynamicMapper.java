package jcowgill.embs;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.actor.util.Time;
import ptolemy.data.IntToken;
import ptolemy.data.RecordToken;
import ptolemy.data.Token;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

public class DynamicMapper extends TypedAtomicActor
{
	private final TypedIOPort input, output;

	private final Core[] cores = {
			new Core(CoreType.CPU),
			new Core(CoreType.CPU),
			new Core(CoreType.DSP),
			new Core(CoreType.HW)
	};

	public DynamicMapper(CompositeEntity container, String name) throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		input = new TypedIOPort(this, "input", true, false);
		input.setTypeEquals(BaseType.GENERAL);
		input.setMultiport(true);

		output = new TypedIOPort(this, "output", false, true);
		output.setMultiport(true);
		output.setTypeEquals(BaseType.GENERAL);
	}

	@Override
	public void fire() throws IllegalActionException
	{
		for(int i = 0; i < input.getWidth(); i++)
		{
			if(input.hasToken(i))
			{
				// Read task and map it to a processor
				output.send(i, processTask((RecordToken) input.get(i)));
			}
		}
	}

	/** Construct a RecordToken with a single field
	 * @throws IllegalActionException */
	private static RecordToken makeSingleRecord(String name, Token token) throws IllegalActionException
	{
		return new RecordToken(new String[] { name }, new Token[] { token });
	}

	/** Processes a task and returns the new token to send onwards
	 * @throws IllegalActionException */
	private Token processTask(RecordToken task) throws IllegalActionException
	{
		// Read communication data
		RecordToken comm = (RecordToken) task.get("communication");

		// Find best core to map to
		Time currentTime = getDirector().getModelTime();
		int bestCoreId = -1;
		int bestCoreTime = Integer.MAX_VALUE;

		for (int id = 0; id < cores.length; id++)
		{
			Core core = cores[id];

			// Note the <=, later cores override previous ones
			if (core.isIdle(currentTime) && core.getTaskTime(task) <= bestCoreTime)
			{
				bestCoreId = id;
				bestCoreTime = core.getTaskTime(task);
			}
		}

		// If there are no free cores, map to CPU1
		if (bestCoreId == -1)
		{
			bestCoreId = 0;
			bestCoreTime = cores[0].getTaskTime(task);
		}

		// Update what the core is doing
		cores[bestCoreId].currentTaskEnd = currentTime.add(bestCoreTime);

		// Return new token
		return RecordToken.merge(
				makeSingleRecord("communication",
						RecordToken.merge(
								makeSingleRecord("destination", new IntToken(bestCoreId)),
								comm)),
				task);
	}

	/** Type of each core */
	private enum CoreType
	{
		CPU,
		DSP,
		HW
	}

	/** Data for a core */
	private static class Core
	{
		public final CoreType type;
		public Time currentTaskEnd;

		public Core(CoreType initialType)
		{
			this.type = initialType;
		}

		/** Returns true if the core is idle */
		public boolean isIdle(Time currentTime)
		{
			return currentTaskEnd == null ||
					currentTime.subtract(currentTaskEnd).getDoubleValue() >= 0;
		}

		/** Returns the time a task will take on this core */
		public int getTaskTime(RecordToken task)
		{
			return ((IntToken) task.get("comptime" + type.name())).intValue();
		}
	}
}
