package jcowgill.embs;

import java.util.Map;
import java.util.TreeMap;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.data.BooleanToken;
import ptolemy.data.expr.Parameter;
import ptolemy.data.type.BaseType;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.Relation;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

/**
 * A master of manu TunnelEndpoints
 */
public class TunnelMaster extends TypedAtomicActor
{
	private final TypedIOPort input, output, tunnelIn, tunnelOut;

	/** Creates a TunnelEndpoint */
	@SuppressWarnings("unused")
	public TunnelMaster(CompositeEntity container, String name) throws IllegalActionException, NameDuplicationException
	{
		super(container, name);

		// Create ports
		input = new TypedIOPort(this, "input", true, false);
		input.setTypeEquals(BaseType.GENERAL);
		input.setMultiport(true);

		output = new TypedIOPort(this, "output", false, true);
		output.setTypeEquals(BaseType.GENERAL);

		tunnelIn = new TypedIOPort(this, "tunnelIn", true, false);
		tunnelIn.setTypeEquals(BaseType.GENERAL);
		tunnelIn.setMultiport(true);

		tunnelOut = new TypedIOPort(this, "tunnelOut", false, true);
		tunnelOut.setTypeEquals(BaseType.GENERAL);
		tunnelOut.setMultiport(true);

		// Hide the tunnel
		new Parameter(tunnelIn, "_hide", BooleanToken.TRUE);
		new Parameter(tunnelOut, "_hide", BooleanToken.TRUE);
	}

	@Override
	public void fire() throws IllegalActionException
	{
		// Forward inputs to the tunnel, and tunnel to the output
		for (int i = 0; i < tunnelIn.getWidth(); i++)
		{
			while (tunnelIn.hasToken(i))
				output.send(0, tunnelIn.get(i));

			while (input.hasToken(i))
				tunnelOut.send(i, input.get(i));
		}
	}

	@Override
	public void preinitialize() throws IllegalActionException
	{
		super.preinitialize();
		updateLinks();
	}

	@Override
	public void wrapup() throws IllegalActionException
	{
		super.wrapup();
		removeLinks();
	}

	@Override
	public void pruneDependencies()
	{
		super.pruneDependencies();
		removeDependency(input, tunnelIn);
		removeDependency(output, tunnelOut);
	}

	/** Removes all tunnel links */
	private void removeLinks()
	{
		for (Object relation : tunnelIn.linkedRelationList())
			((Relation) relation).unlinkAll();
		for (Object relation : tunnelOut.linkedRelationList())
			((Relation) relation).unlinkAll();
		tunnelIn.unlinkAll();
		tunnelOut.unlinkAll();
	}

	/** Updates the links to the endpoints */
	private void updateLinks() throws IllegalActionException
	{
		// Remove all old links
		removeLinks();

		// Get list of endpoints sorted by id
		Map<Integer, TunnelEndpoint> endpoints = new TreeMap<>();
		for (Object actor : ((CompositeEntity) getContainer()).deepEntityList())
		{
			if (actor instanceof TunnelEndpoint)
			{
				TunnelEndpoint endpoint = (TunnelEndpoint) actor;
				Integer id = endpoint.getId();

				if (endpoints.containsKey(id))
					throw new IllegalActionException("Duplicate endpoint ID " + id);

				endpoints.put(id, endpoint);
			}
		}

		// Ensure there are no gaps
		for (int i = 0; i < endpoints.size(); i++)
		{
			if (!endpoints.containsKey(i))
				throw new IllegalActionException("ID does not exist " + i);
		}

		// Link to the tunnel (in order)
		for (TunnelEndpoint endpoint : endpoints.values())
			endpoint.attachMaster(tunnelIn, tunnelOut);
	}
}
