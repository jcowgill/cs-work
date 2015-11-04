package jcowgill.embs;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.data.Token;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

import java.util.Random;

/**
 * Discards 20% of input tokens
 */
public class Discard20Percent extends TypedAtomicActor {
    private final TypedIOPort input, output;
    private final Random random = new Random();

    public Discard20Percent(CompositeEntity container, String name) throws
            IllegalActionException, NameDuplicationException {
        super(container, name);
        input = new TypedIOPort(this, "input", true, false);
        output = new TypedIOPort(this, "output", false, true);
    }

    @Override
    public void fire() throws IllegalActionException {
        Token token = input.get(0);

        if (random.nextInt(5) > 0)
            output.send(0, token);
    }
}
