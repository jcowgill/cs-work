package jcowgill.embs;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.data.IntToken;
import ptolemy.data.RecordToken;
import ptolemy.data.Token;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

/**
 * Maps input tasks to NoC cores via subscribers
 *
 * This is a composite actor with no director, which accepts events
 * but uses internal publishers to distribute them.
 */
public class NocMapper extends TypedAtomicActor
{
	/** Mappings */
	private static final int NOC_WIDTH = 4;
	private static final String MAPPING = "00 03 12 13 21 22 00 20 11 20 23 30 01 31 32 12 02 10 03 11 33";
	//private static final String MAPPING = "01 02 30 11 22 31 32 33 00 23 33 10 21 13 03 12 23 11 20 21 22";
	//private static final String MAPPING = "30 22 02 10 20 31 32 00 03 02 21 11 33 12 22 33 01 00 13 31 23";

	/** Derived values (derived from chosen mapping) */
	private static final int[] MAPPING_X, MAPPING_Y;
	private static final int TASKS;

	/** Input and output ports */
	private final TypedIOPort input, output;

	static
	{
		// Calculate derived values
		String[] splitMapping = MAPPING.split(" ");

		TASKS = splitMapping.length;
		MAPPING_X = new int[TASKS];
		MAPPING_Y = new int[TASKS];

		for (int i = 0; i < TASKS; i++)
		{
			MAPPING_X[i] = Character.getNumericValue(splitMapping[i].charAt(0));
			MAPPING_Y[i] = Character.getNumericValue(splitMapping[i].charAt(1));
		}
	}

	/** Initializes the NocMapper
	 * @throws NameDuplicationException
	 * @throws IllegalActionException */
	public NocMapper(CompositeEntity container, String name) throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		// Create ports
		input = new TypedIOPort(this, "input", true, false);
		input.setTypeEquals(BaseType.GENERAL);
		output = new TypedIOPort(this, "output", false, true);
		output.setMultiport(true);
		output.setTypeEquals(BaseType.GENERAL);
	}

	/** Converts a core id to an endpoint id (for the TunnelMaster) */
	private static int calcEndpointId(int id)
	{
		return NOC_WIDTH * MAPPING_Y[id] + MAPPING_X[id];
	}

	// Dispatch the given task
	private void dispatchTask(RecordToken token) throws IllegalActionException
	{
		// IN:  comptime, id, releasetime, period, messagelength, destination
		// OUT: comptime, x, y, id, releasetime, period, messagelength

		// Get source and destination values
		int src = ((IntToken) token.get("id")).intValue();
		int dest = ((IntToken) token.get("destination")).intValue();

		// Replace destination with x and y coordinates
		String[] labels = {"comptime", "x", "y", "id", "releasetime", "period", "messagelength"};
		Token[] newTokens =
		{
			token.get("comptime"),
			new IntToken(MAPPING_X[dest]),
			new IntToken(MAPPING_Y[dest]),
			token.get("id"),
			token.get("releasetime"),
			token.get("period"),
			token.get("messagelength"),
		};

		RecordToken newToken = new RecordToken(labels, newTokens);

		// Forward to relevant publisher
		output.send(calcEndpointId(src), newToken);
	}

	@Override
	public void fire() throws IllegalActionException
	{
		// Process each task
		while (input.hasToken(0))
			dispatchTask((RecordToken) input.get(0));
	}
}
