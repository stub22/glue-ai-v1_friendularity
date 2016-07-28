package org.friendularity.bundle.headless.speech;

import org.appdapter.core.log.BasicDebugger;
import org.jflux.api.core.Adapter;
import org.jflux.api.core.util.EmptyAdapter;
import org.jflux.api.messaging.rk.services.ServiceCommand;
import org.jflux.api.messaging.rk.services.ServiceError;
import org.jflux.impl.messaging.rk.JMSAvroServiceFacade;
import org.jflux.impl.messaging.rk.ServiceCommandRecord;
import org.jflux.impl.messaging.rk.ServiceErrorRecord;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroAsyncReceiverLifecycle;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroMessageSenderLifecycle;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.mechio.api.speech.SpeechConfig;
import org.mechio.api.speech.SpeechEventList;
import org.mechio.api.speech.SpeechRequest;
import org.mechio.api.speech.SpeechRequestFactory;
import org.mechio.api.speech.SpeechService;
import org.mechio.api.speech.lifecycle.RemoteSpeechServiceClientLifecycle;
import org.mechio.api.speech.viseme.lifecycle.VisemeEventNotifierLifecycle;
import org.mechio.impl.speech.PortableSpeechRequest;
import org.mechio.impl.speech.SpeechConfigRecord;
import org.mechio.impl.speech.SpeechEventListRecord;
import org.mechio.impl.speech.SpeechRequestRecord;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

import java.util.Properties;

/**
 * Code copied here from Activator by Stub22 on 7/27/2016.
 */
public class SpeechServiceLauncher extends BasicDebugger {
	private final static Logger theLogger = getLoggerForClass(SpeechServiceLauncher.class);

	private final static String SPEECH_SERVICE_ID = "speechService";
	private final static String CONNECTION_ID = "speechConnection";
	private final static String COMMAND_DEST_ID = "speechCommand";
	private final static String CONFIG_DEST_ID = "speechConfig";
	private final static String ERROR_DEST_ID = "speechError";
	private final static String REQUEST_DEST_ID = "speechRequest";
	private final static String EVENT_DEST_ID = "speechEvent";
	private final static String COMMAND_SENDER_ID = "speechCommand";
	private final static String CONFIG_SENDER_ID = "speechConfig";
	private final static String ERROR_RECEIVER_ID = "speechError";
	private final static String REQUEST_SENDER_ID = "speechRequest";
	private final static String EVENT_RECEIVER_ID = "speechEvent";

	static public boolean theLaunchHasHappened = false;

	public static boolean launchServices(BundleContext bunCtx)  {
		if (theLaunchHasHappened) {
			throw new RuntimeException("Duplicate launch of qpid service conns for speech");
		}
		try {
			SpeechServiceLauncher launcher = new SpeechServiceLauncher();
			launcher.doLaunchServices(bunCtx);
			theLaunchHasHappened = true;
			return true;
		}	catch (Throwable t) {
			theLogger.error("Caught exception", t);
			return false;
		}

	}
	public void doLaunchServices(BundleContext bunCtx) throws Throwable {

		theLogger.info("Launching JMS SpeechService Provider.");
		launchVisemeNotifier(bunCtx, SPEECH_SERVICE_ID);
		launchRemoteClient(bunCtx,
				SPEECH_SERVICE_ID, SPEECH_SERVICE_ID, CONNECTION_ID,
				COMMAND_DEST_ID, COMMAND_SENDER_ID,
				CONFIG_DEST_ID, CONFIG_SENDER_ID,
				ERROR_DEST_ID, ERROR_RECEIVER_ID,
				REQUEST_DEST_ID, REQUEST_SENDER_ID,
				EVENT_DEST_ID, EVENT_RECEIVER_ID);
		startSpeechReqLifeComp(bunCtx);

	}

	private void startSpeechReqLifeComp(BundleContext bunCtx) {
		new OSGiComponent(bunCtx,
				new SimpleLifecycle(new PortableSpeechRequest.Factory(),
						SpeechRequestFactory.class)).start();

	}

	private void launchVisemeNotifier(
			BundleContext context, String speechServiceId){
		theLogger.info("Launching Dynamic VisemeNotifier Service.");
		new OSGiComponent(context,
				new VisemeEventNotifierLifecycle(speechServiceId)
		).start();
		theLogger.info("Dynamic VisemeNotifier Service Launched.");
	}

	private void launchRemoteClient(BundleContext context,
									String speechClientId, String speechHostId, String connectionId,
									String commandDestId, String commandSenderId,
									String configDestId, String configSenderId,
									String errorDestId, String errorReceiverId,
									String speechReqDestId, String speechRequestSenderId,
									String speechEventsDestId, String speechEventsReceiverId){
		Properties props = new Properties();
		props.put(SpeechService.PROP_ID, speechClientId);

		startClientMessengers(context, props, connectionId,
				commandDestId, commandSenderId,
				configDestId, configSenderId,
				new EmptyAdapter(),
				SpeechConfig.class, SpeechConfigRecord.class,
				errorDestId, errorReceiverId);
		startSpeechMessengers(context, props, connectionId,
				speechReqDestId, speechRequestSenderId,
				speechEventsDestId, speechEventsReceiverId);
		launchRemoteSpeechClient(context, speechClientId, speechHostId,
				commandSenderId, configSenderId, errorReceiverId,
				speechRequestSenderId, speechEventsReceiverId);
	}

	private void launchRemoteSpeechClient(BundleContext context,
										  String speechClientId, String speechHostId,
										  String commandSenderId, String configSenderId,
										  String errorReceiverId, String speechRequestSenderId,
										  String speechEventsReceiverId){
		RemoteSpeechServiceClientLifecycle lifecycle =
				new RemoteSpeechServiceClientLifecycle(speechClientId,
						speechHostId, commandSenderId, configSenderId, errorReceiverId,
						speechRequestSenderId, speechEventsReceiverId);
		OSGiComponent speechComp = new OSGiComponent(context, lifecycle);
		speechComp.start();
	}

	private static <Msg,Rec> void startClientMessengers(BundleContext context,
														Properties groupProps, String connectionId,
														String commandDestId, String commandSenderId,
														String configDestId, String configSenderId,
														Adapter<Msg,Rec> configMsgRecAdapter,
														Class<Msg> msgClass, Class<Rec> recClass,
														String errorDestId, String errorReceiverId){
		JMSAvroMessageSenderLifecycle commandSender =
				new JMSAvroMessageSenderLifecycle(
						new EmptyAdapter(),
						ServiceCommand.class, ServiceCommandRecord.class,
						commandSenderId, connectionId, commandDestId,
						JMSAvroServiceFacade.COMMAND_MIME_TYPE);
		new OSGiComponent(context, commandSender, groupProps).start();

		JMSAvroMessageSenderLifecycle configSender =
				new JMSAvroMessageSenderLifecycle(
						configMsgRecAdapter, msgClass, recClass,
						configSenderId, connectionId, configDestId,
						JMSAvroServiceFacade.CONFIG_MIME_TYPE);
		new OSGiComponent(context, configSender, groupProps).start();

		JMSAvroAsyncReceiverLifecycle errorReceiver =
				new JMSAvroAsyncReceiverLifecycle(
						new EmptyAdapter(),
						ServiceError.class, ServiceErrorRecord.class,
						ServiceErrorRecord.SCHEMA$, errorReceiverId,
						connectionId, errorDestId);
		new OSGiComponent(context, errorReceiver, groupProps).start();
	}

	private static void startSpeechMessengers(BundleContext context,
											  Properties groupProps, String connectionId,
											  String speechReqDestId, String speechRequestSenderId,
											  String speechEventsDestId, String speechEventsReceiverId){

		JMSAvroMessageSenderLifecycle speechRequestSender =
				new JMSAvroMessageSenderLifecycle(
						new EmptyAdapter(),
						SpeechRequest.class, SpeechRequestRecord.class,
						speechRequestSenderId, connectionId, speechReqDestId);
		new OSGiComponent(context, speechRequestSender, groupProps).start();

		JMSAvroAsyncReceiverLifecycle speechEventsReceiver =
				new JMSAvroAsyncReceiverLifecycle(
						new EmptyAdapter(),
						SpeechEventList.class, SpeechEventListRecord.class,
						SpeechEventListRecord.SCHEMA$, speechEventsReceiverId,
						connectionId, speechEventsDestId);
		new OSGiComponent(context, speechEventsReceiver, groupProps).start();
	}

}
