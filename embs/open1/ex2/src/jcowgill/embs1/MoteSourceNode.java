package jcowgill.embs1;

import com.ibm.saguaro.logger.Logger;
import com.ibm.saguaro.system.Assembly;
import com.ibm.saguaro.system.DevCallback;
import com.ibm.saguaro.system.Device;
import com.ibm.saguaro.system.LED;
import com.ibm.saguaro.system.Mote;
import com.ibm.saguaro.system.Radio;
import com.ibm.saguaro.system.SystemInfo;
import com.ibm.saguaro.system.Time;
import com.ibm.saguaro.system.Timer;
import com.ibm.saguaro.system.TimerEvent;
import com.ibm.saguaro.system.csr;

/**
 * A Source node implemented with MoteRunner
 *
 * LED Colours:
 * - Yellow = Turned on constantly after everything is initialized
 * - Green  =
 * - Red    =
 */
public final class MoteSourceNode
{
	/* LED Colours */
	private static final byte LED_YELLOW = (byte) 0;
	private static final byte LED_GREEN = (byte) 1;
	private static final byte LED_RED = (byte) 2;

	/* LED States */
	private static final byte LED_OFF = (byte) 0;
	private static final byte LED_ON = (byte) 1;

	/** Number of channels */
	private static final int CHANNELS = 3;

	/** Number added to internal channels to get the external channel number */
	private static final int EXT_CHANNEL_OFFSET = 11;

	/** The controller for the source node */
	private static final SourceController controller;

	/** Radio reference */
	private static final Radio radio = new Radio();

	/** Wakeup timer */
	private static final Timer timer = new Timer();

	static
	{
		// Register unload callback
		Assembly.setSystemInfoCallback(new SystemInfo(null)
		{
			@Override
			public int invoke(int type, int info) {
				return onUnload(type, info);
			}
		});

		// Open the radio
		radio.open(Radio.DID, null, 0, 0);

		// Register radio and timer callbacks
		radio.setRxHandler(new DevCallback(null)
		{
			@Override
			public int invoke(int flags, byte[] data, int len, int info, long time) {
				return rxBeacon(flags, data, len, info, time);
			}
		});

		radio.setTxHandler(new DevCallback(null)
		{
			@Override
			public int invoke(int flags, byte[] data, int len, int info, long time) {
				return txComplete(flags, data, len, info, time);
			}
		});

		timer.setCallback(new TimerEvent(null)
		{
			@Override
			public void invoke(byte param, long time) {
				timerFire(param, time);
			}
		});

		// Initialize the source controller
		controller = new SourceController(CHANNELS, Time.currentTime(Time.MILLISECS));

		// Enter read mode

		// Turn Yellow LED on once we've finished
		LED.setState(LED_YELLOW, LED_ON);
	}

	/**
	 * Attempts to change the channel of the radio
	 *
	 * If the radio receiver is enabled, it will request it to be turned off.
	 *
	 * @return true if the channel was changed immediately, false if it's pending
	 */
	private boolean trySetChannel(int channel)
	{
		// Calculate radio channel
		byte radioChannel = (byte) (channel + EXT_CHANNEL_OFFSET);

		// Don't do anything if the channel is correct
		if (radio.getChannel() == radioChannel)
			return true;

		Logger.appendString(csr.s2b("trySetChannel Radio State = "));
		Logger.appendInt(radio.getState());
		Logger.flush(Mote.INFO);

		switch (radio.getState())
		{
		case Device.S_ACTING:
		case Device.S_RXSTP:
		case Device.S_RXSTPTXING:
			// Transmitting, do nothing until transmit has completed
			return false;

		case Device.S_ENABLING:
		case Device.S_RXEN:
		case Device.S_RXTXING:
		case Device.S_RXTXPEND:
			// Receiving, stop rx and wait
			radio.stopRx();
			return false;

		default:
			// Try to change it immediately in the other states
			radio.setChannel(radioChannel);
			return true;
		}

		/*
		 *
		 * Impossible:
		 * S_CANCEL
		 * S_CLOSED
		 * S_FLAGS_MASK
		 * S_IRQPEND
		 * S_OFF
		 * S_STATE_MASK
		 * S_STDBY
		 *
		 * stopRx:
		 * S_ENABLING		Read enabling
		 * S_RXEN			Read is enabled
		 * S_RXTXING		Transmitting, then read enabled
		 * S_RXTXPEND		Reading, then transmitting, then read enabled
		 *
		 * wait:
		 * S_ACTING			Transmitting, then OK
		 * S_RXSTP			Read stopping
		 * S_RXSTPTXING		Transmitting, then read stopping
		 *
		 * do now:
		 * S_ACTIVE			OK
		 * S_ACTIVATING		STDBY -> ACTIVE
		 * S_WARMINGUP		OFF -> STDBY
		 */
	}

	/** Assembly unload callback */
	static int onUnload(int type, int info)
	{
		// Actions to perform when assembly is unloaded
		if (type == Assembly.SYSEV_DELETED)
		{
			radio.close();
			LED.setState(LED_YELLOW, LED_OFF);
			LED.setState(LED_GREEN, LED_OFF);
			LED.setState(LED_RED, LED_OFF);
		}

		return 0;
	}

	/** Timer callback */
	static void timerFire(byte param, long time)
	{
		// TODO Auto-generated method stub

	}

	/** Radio beacon received callback */
	static int rxBeacon(int flags, byte[] data, int len, int info, long time)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	/** Radio transmit complete callback */
	static int txComplete(int flags, byte[] data, int len, int info, long time)
	{
		// TODO Auto-generated method stub
		return 0;
	}
}
