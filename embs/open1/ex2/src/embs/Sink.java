package embs;

import com.ibm.saguaro.system.*;
import com.ibm.saguaro.logger.*;

public class Sink {

	private static final int PAN_ID_OFFSET = 0x11;

	private Timer  tsend;
	private Timer  tstart;
	private boolean light=false;

	private byte[] xmit;
	private long	  wait;
	private Radio radio = new Radio();
	private int nc;

	// settings for sink A
	private final int n;
	private final byte channel;

	public Sink() {
		// Default settings
		this(0, 8, 600);
	}

	public Sink(int inChannel, int inN, int t) {
		// Store arguments
		this.channel = (byte) inChannel;
		this.n = inN;

		// Open the default radio
		radio.open(Radio.DID, null, 0, 0);

		// Set channel
		radio.setChannel(channel);

		// Set the PAN ID and the short address
		int panid = PAN_ID_OFFSET + channel;
		radio.setPanId(panid, true);
		radio.setShortAddr(panid);

		// Prepare beacon frame with source and destination addressing
		xmit = new byte[12];
		xmit[0] = Radio.FCF_BEACON;
		xmit[1] = Radio.FCA_SRC_SADDR|Radio.FCA_DST_SADDR;
		Util.set16le(xmit, 3, panid); // destination PAN address
		Util.set16le(xmit, 5, 0xFFFF); // broadcast address
		Util.set16le(xmit, 7, panid); // own PAN address
		Util.set16le(xmit, 9, panid); // own short address

		xmit[11] = (byte)n;

		// register delegate for received frames
		radio.setRxHandler(new DevCallback(this){
				@Override
				public int invoke (int flags, byte[] data, int len, int info, long time) {
					return ((Sink) obj).onReceive(flags, data, len, info, time);
				}
			});

		// Setup a periodic timer callback for beacon transmissions
		tsend = new Timer();
		tsend.setCallback(new TimerEvent(this){
				@Override
				public void invoke(byte param, long time){
					((Sink) obj).periodicSend(param, time);
				}
			});

		// Setup a periodic timer callback to restart the protocol
		tstart = new Timer();
		tstart.setCallback(new TimerEvent(this){
				@Override
				public void invoke(byte param, long time){
					((Sink) obj).restart(param, time);
				}
			});

		// Convert the periodic delay from ms to platform ticks
		wait = Time.toTickSpan(Time.MILLISECS, t);

		tstart.setAlarmBySpan(Time.toTickSpan(Time.SECONDS, 5)); //starts the protocol 5 seconds after constructing the assembly
	}

	// Called when a frame is received or at the end of a reception period
	int onReceive (int flags, byte[] data, int len, int info, long time) {
		if (data == null) { // marks end of reception period
			// turn green LED off
			LED.setState((byte)1, (byte)0);

			//set alarm to restart protocol
			tstart.setAlarmBySpan(10*wait);
			return 0;
		}

		// frame received, so blink red LED and log its payload
		if(light){
			LED.setState((byte)2, (byte)1);
		}
		else{
			LED.setState((byte)2, (byte)0);
		}
		light=!light;

		Logger.appendByte(data[11]);
		Logger.flush(Mote.WARN);
		return 0;
	}

	// Called on a timer alarm
	void periodicSend(byte param, long time) {
		if(nc>0){
			// transmit a beacon
			radio.transmit(Device.ASAP|Radio.TXMODE_POWER_MAX, xmit, 0, 12, 0);
			// program new alarm
			tsend.setAlarmBySpan(wait);
			nc--;
			xmit[11]--;
		}
		else{
			//start reception phase
			radio.startRx(Device.ASAP, 0, Time.currentTicks()+wait);
			// turn green LED on
			LED.setState((byte)1, (byte)1);
		}
	}

	// Called on a timer alarm, starts the protocol
	void restart(byte param, long time) {
		nc=n;
		xmit[11]=(byte)n;
		tsend.setAlarmBySpan(0);
	}
}
