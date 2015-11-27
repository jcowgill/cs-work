package lsi.wsn.sync;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.util.Time;
import ptolemy.data.IntToken;
import ptolemy.data.expr.Parameter;
import ptolemy.data.type.BaseType;
import ptolemy.domains.wireless.kernel.WirelessIOPort;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;
import ptolemy.vergil.icon.EditorIcon;
import ptolemy.vergil.kernel.attributes.EllipseAttribute;

public class TDMAnchor extends TypedAtomicActor{

	//ports
	protected WirelessIOPort input;
	protected WirelessIOPort output;

	protected Parameter pt; // time interval between beacons
	protected Parameter pn; // number of beacons of sync phase
	protected int state; // protocol state

	private double t; // private variable storing the parameter t, updated upon initialisation
	private int n; // private variable storing the parameter n, updated upon initialisation
	private int syncCount; // private variable to control the number of remaining beacon frames to be sent within an iteration of the protocol

	// constants to denote LED colours
	public static final int BLACK=0;
	public static final int GREEN=1;
	public static final int RED=2;

	// constants to denote protocol state
	public static final int SLEEP=100; // sleep phase
	public static final int SYNC=101;  // synchronisation phase
	public static final int RX=102;    // reception phase

	protected Time nextFire; // time of the next scheduled firing
	protected IntToken beaconFrame; // token representing the frame to be transmitted

	// icon related
	protected String iconColor = "{0.0, 0.0, 0.0, 1.0}"; // black, LED off by default
	protected EllipseAttribute _circle;
	protected EditorIcon node_icon;

	public TDMAnchor(CompositeEntity container, String name)
			throws NameDuplicationException, IllegalActionException  {

		super(container, name);

		//port instantiation, setting default channels and types
		input = new WirelessIOPort(this, "input", true, false);
		output = new WirelessIOPort(this, "output", false, true);
		input.outsideChannel.setExpression("Channel1");
		output.outsideChannel.setExpression("Channel1");

		input.setTypeEquals(BaseType.INT);
		output.setTypeEquals(BaseType.INT);

		//parameter instantiation, setting default values
		pt = new Parameter(this,"t");
		pt.setExpression("0.5");

		pn = new Parameter(this,"n");
		pn.setExpression("5");

		//icon related
		buildIcon();
	}

	public void initialize() throws IllegalActionException {
		super.initialize();

		state = TDMAnchor.SLEEP; // start at SLEEP mode
		this.setLED(TDMAnchor.BLACK); // turn off the LED

		//read parameters
		t = Double.parseDouble(pt.getExpression());
		n = Integer.parseInt(pn.getExpression());

		//reset counter
		syncCount=0;

		// schedule the first firing at the end of the first sleep period
		nextFire = getDirector().getModelTime().add(10*t);
		getDirector().fireAt(this, nextFire);
	}

	public void fire() throws IllegalActionException{
		//stores current time
		Time curTime = getDirector().getModelTime();

		//checks whether this is a scheduled firing by comparing current time with the scheduled time for the next firing
		if(curTime.compareTo(nextFire)!=-1){
			if(syncCount==0)
				changeState(); // if scheduled firing, change state (unless this is an intermediate sync firing)
			else
				sendBeacon(); // if this is an intermediate sync firing, transmit beacon frame
		}

		if(input.hasToken(0)){  // if another node has transmitted
			//read token
			input.get(0);

			if(state==TDMAnchor.RX) // if in receiving period
				// turn on the green LED
				this.setLED(TDMAnchor.GREEN);
			else
				// turn on the red LED
				this.setLED(TDMAnchor.RED);
		}
	}

	// main protocol state machine
	protected void changeState() throws IllegalActionException{
		if(state==TDMAnchor.SLEEP){ // transition from sleep to sync, set number of sync beacons, triggers the first beacon transmission
			state=TDMAnchor.SYNC;
			syncCount=n;
			sendBeacon();

		} else if(state==TDMAnchor.SYNC){ // transition from sync to rx
			state = TDMAnchor.RX;
			nextFire = getDirector().getModelTime().add(t); // next scheduled firing in t time units
			getDirector().fireAt(this, nextFire);

		} else if(state==TDMAnchor.RX){ // transition from rx to sleep
			this.setLED(TDMAnchor.BLACK);			// turn off the LED

			state = TDMAnchor.SLEEP;
			nextFire = getDirector().getModelTime().add(10*t); // next scheduled firing in 10*t time units
			getDirector().fireAt(this, nextFire);
		}
	}

	//send out beacon frame
	protected void sendBeacon() throws IllegalActionException {
		beaconFrame = new IntToken(syncCount); // sends out a beacon with payload as specified by the protocol
		output.send(0, beaconFrame);
		syncCount--; // decrement counter
		nextFire = getDirector().getModelTime().add(t);
		getDirector().fireAt(this, nextFire); // schedule next firing
	}

	// change the filling colour of the icon
	protected void setLED(int state) throws IllegalActionException {
		if(state == TDMAnchor.RED)
			_circle.fillColor.setToken("{1.0, 0.0, 0.0, 1.0}"); // red
		else if(state == TDMAnchor.GREEN)
			_circle.fillColor.setToken("{0.0, 1.0, 0.0, 1.0}"); // green
		else
			_circle.fillColor.setToken("{0.0, 0.0, 0.0, 1.0}"); // black
	}

	// set the actor icon as a 20x20 pixel black circle
	protected void buildIcon() throws IllegalActionException, NameDuplicationException {
		node_icon = new EditorIcon(this, "_icon");
		_circle = new EllipseAttribute(node_icon, "_circle");
		_circle.centered.setToken("true");
		_circle.width.setToken("20");
		_circle.height.setToken("20");
		_circle.fillColor.setToken(this.iconColor);
		_circle.lineColor.setToken("{0.0, 0.0, 0.0, 1.0}");
		node_icon.setPersistent(false);
	}
}
