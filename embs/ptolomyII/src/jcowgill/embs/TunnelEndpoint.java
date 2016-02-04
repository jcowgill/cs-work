package jcowgill.embs;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedCompositeActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.actor.TypedIORelation;
import ptolemy.data.BooleanToken;
import ptolemy.data.IntToken;
import ptolemy.data.expr.Parameter;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;
import ptolemy.kernel.util.NamedObj;

/**
 * An endpoint of TunnelMaster
 */
public class TunnelEndpoint extends TypedAtomicActor
{
	private final TypedIOPort input, output, tunnelIn, tunnelOut;
	private final Parameter id;

	/** Creates a TunnelEndpoint */
	public TunnelEndpoint(CompositeEntity container, String name) throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		// Create ports
		input = new TypedIOPort(this, "input", true, false);
		output = new TypedIOPort(this, "output", false, true);
		tunnelIn = new TypedIOPort(this, "tunnelIn", true, false);
		tunnelOut = new TypedIOPort(this, "tunnelOut", false, true);

		// Hide the tunnel
		hideObject(tunnelIn);
		hideObject(tunnelOut);

		// Create id parameter
		id = new Parameter(this, "id", IntToken.ZERO);
		id.setTypeEquals(BaseType.INT);
	}

	/** Returns the ID of this endpoint */
	public int getId() throws IllegalActionException
	{
		return ((IntToken) id.getToken()).intValue();
	}

	/** Attach this endpoint to a master
	 * @throws IllegalActionException
	 * @throws NameDuplicationException */
	public void attachMaster(TypedIOPort masterIn, TypedIOPort masterOut) throws IllegalActionException
	{
		TypedCompositeActor container = (TypedCompositeActor) getContainer();

		try
		{
			// Input relation
			TypedIORelation relIn = new TypedIORelation(container, container.uniqueName("tunnelRelIn"));
			relIn.setPersistent(false);
			hideObject(relIn);
			masterOut.link(relIn);
			tunnelIn.link(relIn);

			// Output relation
			TypedIORelation relOut = new TypedIORelation(container, container.uniqueName("tunnelRelOut"));
			relOut.setPersistent(false);
			hideObject(relOut);
			masterIn.link(relOut);
			tunnelOut.link(relOut);
		}
		catch (NameDuplicationException e)
		{
			throw new IllegalActionException("duplicate name exception");
		}
	}

	@Override
	public void fire() throws IllegalActionException
	{
		// Forward inputs to the tunnel, and tunnel to the output
		while (tunnelIn.hasToken(0))
			output.send(0, tunnelIn.get(0));

		while (input.hasToken(0))
			tunnelOut.send(0, input.get(0));
	}

	@Override
	public void pruneDependencies()
	{
		super.pruneDependencies();
		removeDependency(input, tunnelIn);
		removeDependency(output, tunnelOut);
	}

	/** Hides a named object
	 * @throws NameDuplicationException
	 * @throws IllegalActionException */
	@SuppressWarnings("unused")
	private static void hideObject(NamedObj object) throws IllegalActionException, NameDuplicationException
	{
		new Parameter(object, "_hide", BooleanToken.TRUE);
	}
}
