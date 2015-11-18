package jcowgill.embs1;

import com.ibm.saguaro.system.Assembly;
import com.ibm.saguaro.system.DevCallback;
import com.ibm.saguaro.system.Device;
import com.ibm.saguaro.system.LED;
import com.ibm.saguaro.system.Radio;
import com.ibm.saguaro.system.SystemInfo;
import com.ibm.saguaro.system.Time;
import com.ibm.saguaro.system.Timer;
import com.ibm.saguaro.system.TimerEvent;
import com.ibm.saguaro.system.Util;

/** A Source node implemented with MoteRunner */
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

	/** Number to add to the channel id to get the PAN id */
	private static final int PAN_ID_OFFSET = 0x11;

	/** The short address of this source node */
	private static final int MY_SHORT_ADDRESS = 0x42;

	/** The controller for the source node */
	private static final SourceController controller;

	/** Radio reference */
	private static final Radio radio = new Radio();

	/** Wakeup timer */
	private static final Timer timer = new Timer();

	/** Transmit buffer */
	private static final byte[] xmit = new byte[12];

	/** If true, we are currently receiving or transmitting */
	private static boolean rxOn, txOn;

	/**
	 * Channel to send on first.
	 *
	 * If we try to send on a channel which we are not currently on (fairly
	 * likely), this variable is filled with the channel number so we know what
	 * it is after we've finished changing the channel.
	 *
	 * This variable is -1 if there is no pending send channel (ie ask the
	 * controller for one instead).
	 */
	private static int pendingSendChannel = -1;

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

		// Setup xmit fields which do not change
		xmit[0] = Radio.FCF_DATA;
		xmit[1] = Radio.FCA_SRC_SADDR | Radio.FCA_DST_SADDR;
		Util.set16le(xmit, 9, MY_SHORT_ADDRESS);

		// Initialize the source controller
		controller = new SourceController(CHANNELS, Time.currentTime(Time.MILLISECS));

		// Enter read mode and enable timer
		radio.setShortAddr(MY_SHORT_ADDRESS);
		handleControllerStateChange();

		// Turn Yellow LED on once we've finished
		LED.setState(LED_YELLOW, LED_ON);
	}

	/** Assembly unload callback */
	static int onUnload(int type, int info)
	{
		// Actions to perform when assembly is unloaded
		if (type == Assembly.SYSEV_DELETED)
		{
			timer.cancelAlarm();
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
		// Send a wakeup event and handle any sends or channel changes
		controller.wakeupEvent(Time.currentTime(Time.MILLISECS));
		handleControllerStateChange();
	}

	/** Radio beacon received callback */
	static int rxBeacon(int flags, byte[] data, int len, int info, long time)
	{
		// Handle new beacon packets
		if (data != null)
		{
			// If we don't change the channel when sending, there is a small change we
			//  will get a beacon for the wrong channel (which we will ignore).
			if (controller.getReadChannel() != radio.getChannel())
				return 0;

			// Validate beacon
			if (len != 12 || (data[0] & Radio.FCF_BEACON) == 0)
				return 0;

			// Notify controller
			//  We send a wakeupEvent as well to ensure all pending sends are refreshed now
			long absoluteTime = Time.currentTime(Time.MILLISECS);
			int n = data[11];

			controller.receiveBeacon(absoluteTime, n);
			controller.wakeupEvent(absoluteTime);
		}
		else
		{
			// Receiving finished
			rxOn = false;
		}

		// Handle any sends or channel changes
		handleControllerStateChange();
		return 0;
	}

	/** Radio transmit complete callback */
	static int txComplete(int flags, byte[] data, int len, int info, long time)
	{
		txOn = false;
		handleControllerStateChange();
		return 0;
	}

	/** Handles any changes in the controller state (sends and channel hopping) */
	private static void handleControllerStateChange()
	{
		// Reschedule timer event
		timer.cancelAlarm();

		long wakeupTime = controller.getNextWakeupTime();
		if (wakeupTime > 0)
			timer.setAlarmTime(wakeupTime);

		// See if there is anything to transmit
		int sendChannel;
		if (pendingSendChannel != -1)
		{
			sendChannel = pendingSendChannel;
			pendingSendChannel = -1;
		}
		else
		{
			sendChannel = controller.calcSendChannel();
		}

		if (sendChannel != -1)
		{
			// Switch channel and send
			if (tryChangeChannel(sendChannel))
			{
				radio.transmit(Device.ASAP | Radio.TXMODE_POWER_MAX | Radio.TXMODE_CCA,
						xmit, 0, xmit.length, 0);
				txOn = true;
			}
			else
			{
				// Wait for an rxOff or txOff event
				pendingSendChannel = sendChannel;
			}
		}
		else if (tryChangeChannel(controller.getReadChannel()))
		{
			radio.startRx(Device.ASAP | Device.RX4EVER, 0, 0);
			rxOn = true;
		}
	}

	/**
	 * Try and change the radio channel
	 *
	 * If the radio is on, it begins the turn off procedure.
	 *
	 * @param channel channel to change to
	 * @return true if the channel was immediately changed
	 */
	private static boolean tryChangeChannel(int channel)
	{
		// Do nothing if we're already on the right channel
		if (radio.getChannel() == channel)
			return true;

		// We can't do anything if the receiver or transmitter are on
		if (rxOn)
		{
			radio.stopRx();
			return false;
		}

		if (txOn)
			return false;

		// Change the channel
		radio.setChannel((byte) channel);
		radio.setPanId(PAN_ID_OFFSET + channel, true);

		Util.set16le(xmit, 3, PAN_ID_OFFSET + channel); // Dest PAN
		Util.set16le(xmit, 5, PAN_ID_OFFSET + channel); // Dest Address
		Util.set16le(xmit, 7, PAN_ID_OFFSET + channel); // Source PAN
		return true;
	}
}
