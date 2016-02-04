package jcowgill.embs;

import ptolemy.actor.Director;
import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.TypedIOPort;
import ptolemy.actor.util.Time;
import ptolemy.data.DoubleToken;
import ptolemy.data.RecordToken;
import ptolemy.data.Token;
import ptolemy.data.type.BaseType;
import ptolemy.data.type.RecordType;
import ptolemy.data.type.Type;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

/**
 * Models a processor which can only be running one task at once
 */
public class Prac2Processor extends TypedAtomicActor {
    /** Output the utilization after this number of time units */
    private static final double UTILIZATION_PERIOD = 50;

    /** Input and output ports */
    private final TypedIOPort inTasks, outDiscard, outUtilization;

    /** True if the processor is busy */
    private boolean isBusy;

    /** The time the current task starts / finishes. Undefined if isBusy is false. */
    private Time taskStartTime, taskFinishTime;

    /** Number of time units the processor has been utilized since the last trigger */
    private double utilization;

    /** Time to trigger the next utilization output */
    private Time utilizationTrigger;

    public Prac2Processor(CompositeEntity container, String name) throws
            IllegalActionException, NameDuplicationException {
        super(container, name);

        RecordType taskType = new RecordType(new String[] {"comptime"}, new Type[]{ BaseType.DOUBLE });

        inTasks = new TypedIOPort(this, "tasks", true, false);
        inTasks.setMultiport(true);
        inTasks.setTypeEquals(taskType);

        outDiscard = new TypedIOPort(this, "discard", false, true);
        outDiscard.setTypeEquals(taskType);

        outUtilization = new TypedIOPort(this, "utilization", false, true);
        outUtilization.setTypeEquals(BaseType.DOUBLE);
    }

    /** Send utilization and reset trigger */
    private void sendUtilization() throws IllegalActionException {
        Director director = this.getDirector();
        Time currentTime = director.getModelTime();

        // Before we send this, we update the utilization of tasks still running
        if (isBusy) {
            double taskRuntime = currentTime.subtract(taskStartTime).getDoubleValue();
            if (taskRuntime > UTILIZATION_PERIOD)
                taskRuntime = UTILIZATION_PERIOD;
            utilization += taskRuntime;
        }

        outUtilization.send(0, new DoubleToken(utilization));
        utilization = 0;

        utilizationTrigger = currentTime.add(UTILIZATION_PERIOD);
        director.fireAt(this, utilizationTrigger);
    }

    @Override
    public void initialize() throws IllegalActionException {
        // Wipe initial values
        isBusy = false;
        taskStartTime = null;
        taskFinishTime = null;
        utilization = 0;

        // Send first utilization data point
        sendUtilization();
    }

    @Override
    public void fire() throws IllegalActionException {
        Director director = this.getDirector();
        Time currentTime = director.getModelTime();

        // Send utilization if we've reached the trigger
        if (utilizationTrigger.subtract(currentTime).getDoubleValue() <= 0)
            sendUtilization();

        // Finish, if we are due to finish the task
        if (isBusy && taskFinishTime.subtract(currentTime).getDoubleValue() <= 0) {
            // Update utilization before wiping
            Time periodStart = utilizationTrigger.subtract(UTILIZATION_PERIOD);

            int ERROR_THIS_IS_NOT_FINISHED_YET;
            if (true)
                throw new AssertionError();

            isBusy = false;
            taskFinishTime = null;
        }

        // Handle any new tokens
        while (inTasks.hasToken(0)) {
            // Discard if busy, or drop if we can start it
            Token token = inTasks.get(0);

            if (isBusy) {
                outDiscard.send(0, token);
            } else {
                // Mark busy and trigger event when task finishes
                double comptime = ((DoubleToken) ((RecordToken) token).get("comptime")).doubleValue();

                isBusy = true;
                taskFinishTime = currentTime.add(comptime);

                director.fireAt(this, taskFinishTime);
            }
        }
    }
}
