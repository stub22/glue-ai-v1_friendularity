/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.gmteach.ext.vworld;

import java.awt.Image;
import java.net.URISyntaxException;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;
import javax.swing.SwingUtilities;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQTopic;
import org.appdapter.core.log.BasicDebugger;
import org.jflux.api.core.Listener;
import org.jflux.api.core.util.EmptyAdapter;
import org.mechio.api.messaging.services.ServiceCommand;
import org.mechio.api.messaging.services.ServiceCommandFactory;
import org.mechio.api.messaging.services.ServiceError;
import org.mechio.api.vision.ImageEvent;
import org.mechio.api.vision.ImageRegionList;
import org.mechio.api.vision.config.CameraServiceConfig;
import org.mechio.api.vision.config.FaceDetectServiceConfig;
import org.mechio.api.vision.messaging.RemoteImageRegionServiceClient;
import org.mechio.api.vision.messaging.RemoteImageServiceClient;
import org.mechio.impl.messaging.JMSAvroServiceFacade;
import org.mechio.impl.messaging.ServiceCommandRecord;
import org.mechio.impl.messaging.ServiceErrorRecord;
import org.mechio.impl.messaging.services.PortableServiceCommand;
import org.mechio.impl.vision.CameraConfig;
import org.mechio.impl.vision.FaceDetectConfig;
import org.mechio.impl.vision.ImageRecord;
import org.mechio.impl.vision.ImageRegionListRecord;


import org.jflux.api.core.Listener;
import org.jflux.impl.messaging.rk.JMSAvroMessageAsyncReceiver;
import org.jflux.impl.messaging.rk.JMSAvroMessageSender;
import org.mechio.api.common.playable.PlayState;
import org.jflux.api.core.Listener;
import org.mechio.api.common.playable.PlayState;
import static org.mechio.api.common.playable.PlayState.COMPLETED;
import static org.mechio.api.common.playable.PlayState.PAUSED;
import static org.mechio.api.common.playable.PlayState.PENDING;
import static org.mechio.api.common.playable.PlayState.RUNNING;
import static org.mechio.api.common.playable.PlayState.STOPPED;
import org.mechio.api.common.utils.TimeUtils;
import org.mechio.api.vision.ImageEvent;
import org.mechio.api.vision.ImageRegion;
import org.mechio.api.vision.ImageRegionList;
import org.mechio.impl.vision.PortableImageUtils;

/**
 *
 * @author Owner
 */
public class VisionDataFeed extends BasicDebugger {

	private RemoteImageServiceClient myVideoService;
	private RemoteImageRegionServiceClient myFaceService;
	private Image myImage;
	private ImageRegionList<ImageRegion> myImageRegions;
	private Runnable myRepaint;
	private ImageCache myImageCache;
	private ImageRegionListCache myImageRegionListCache;

	class ImageCache implements Listener<ImageEvent> {
		int		myEventCounter;

		@Override public void handleEvent(ImageEvent event) {
			if (event == null) {
				return;
			}
			Image img = PortableImageUtils.unpackImage(event);
			if (img == null) {
				return;
			}
			myImage = img;
			if ((myEventCounter % 100) == 0) {
				getLogger().info("Received image event #" + myEventCounter);
			}
			myEventCounter++;
		//	SwingUtilities.invokeLater(myRepaint);
		}
	}

	class ImageRegionListCache implements Listener<ImageRegionList> {

		int		myEventCounter;
		@Override
		public void handleEvent(ImageRegionList event) {
			if (event == null) {
				return;
			}
			myImageRegions = event;
			if ((myEventCounter % 100) == 0) {
				getLogger().info("Received imageRegions event #" + myEventCounter);
			}
			myEventCounter++;

		}
	}

	public boolean connectServices() {

		String brokerTCP_Addr = "127.0.0.1:5672";
		String brokerAMQP_URL = "amqp://admin:admin@clientid/test?brokerlist='tcp://" + brokerTCP_Addr + "'";
		try {
			myVideoService = connectToVisionImgSvc(brokerAMQP_URL);
			myFaceService = connectToVisionRegionSvc(brokerAMQP_URL);
			if ((myVideoService != null) && (myFaceService != null)) {
				return true;
			}
			//    myVideoControl.setPlayable(videoService);
			//    myFaceDetectControl.setPlayable(faceService);
			//videoService.addImageListener(myVideoPanel.getImageEventListener());
			//faceService.addImageRegionsListener(
			//		myVideoPanel.getImageRegionListListener());
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return false;
	}
	public void registerDummyListeners() {
        myImageCache = new ImageCache();
        myImageRegionListCache = new ImageRegionListCache();
		
		myVideoService.addImageListener(myImageCache);
		myFaceService.addImageRegionsListener(myImageRegionListCache);
	}
	public void startServices() { 
/*       PlayState state = myPlayable.getPlayState();
        switch(state){
            case PENDING:
            case COMPLETED:
            case STOPPED: myPlayable.start(TimeUtils.now()); break;
            case RUNNING: myPlayable.pause(TimeUtils.now()); break;
            case PAUSED: myPlayable.resume(TimeUtils.now()); break;
*/
		myVideoService.start(TimeUtils.now());
		myFaceService.start(TimeUtils.now());
	}
	private RemoteImageServiceClient connectToVisionImgSvc(String brokerAMQP_URL) throws
			URISyntaxException, JMSException, Exception {
		ConnectionFactory cf = new AMQConnectionFactory(brokerAMQP_URL);
		Connection connection = cf.createConnection();
		Session session =
				connection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
		connection.start();

		Destination cmdDest = new AMQQueue(
				"camera0Command; {create: always, node: {type: queue}}");
		Destination errDest = new AMQTopic(
				"camera0Error; {create: always, node: {type: topic}}");
		Destination imgDest = new AMQTopic(
				"camera0Event; {create: always, node: {type: topic}}");

		JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord> cmdSender =
				new JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord>(
				session, cmdDest);
		JMSAvroMessageSender<CameraServiceConfig, CameraConfig> configSender =
				new JMSAvroMessageSender<CameraServiceConfig, CameraConfig>(
				session, cmdDest);
		JMSAvroMessageAsyncReceiver<ServiceError, ServiceErrorRecord> errorReceiver =
				new JMSAvroMessageAsyncReceiver<ServiceError, ServiceErrorRecord>(
				session, errDest,
				ServiceErrorRecord.class, ServiceErrorRecord.SCHEMA$);
		ServiceCommandFactory cmdFactory = new PortableServiceCommand.Factory();
		JMSAvroMessageAsyncReceiver<ImageEvent, ImageRecord> imageReceiver =
				new JMSAvroMessageAsyncReceiver<ImageEvent, ImageRecord>(
				session, imgDest,
				ImageRecord.class, ImageRecord.SCHEMA$);

		RemoteImageServiceClient<CameraServiceConfig> service =
				new RemoteImageServiceClient<CameraServiceConfig>(
				CameraServiceConfig.class, "imageService", "remoteId",
				cmdSender, configSender, errorReceiver,
				cmdFactory, imageReceiver);

		cmdSender.setAdapter(new EmptyAdapter());
		cmdSender.setDefaultContentType(JMSAvroServiceFacade.COMMAND_MIME_TYPE);
		cmdSender.start();

//        configSender.setAdapter(new PortableCameraServiceConfig.MessageRecordAdapter());
//        configSender.setDefaultContentType(JMSAvroServiceFacade.CONFIG_MIME_TYPE);
//        configSender.start();

		errorReceiver.setAdapter(new EmptyAdapter());
		errorReceiver.start();

		imageReceiver.setAdapter(new EmptyAdapter());
		imageReceiver.start();

		return service;
	}

	private RemoteImageRegionServiceClient connectToVisionRegionSvc(String brokerAMQP_URL) throws
			URISyntaxException, JMSException, Exception {
		ConnectionFactory cf = new AMQConnectionFactory(brokerAMQP_URL);
		Connection connection = cf.createConnection();
		Session session =
				connection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
		connection.start();

		Destination cmdDest = new AMQQueue(
				"visionproc0Command; {create: always, node: {type: queue}}");
		Destination errDest = new AMQTopic(
				"visionproc0Error; {create: always, node: {type: topic}}");
		Destination imgRgnDest = new AMQTopic(
				"visionproc0Event; {create: always, node: {type: topic}}");

		JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord> cmdSender =
				new JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord>(
				session, cmdDest);
		JMSAvroMessageSender<FaceDetectServiceConfig, FaceDetectConfig> configSender =
				new JMSAvroMessageSender<FaceDetectServiceConfig, FaceDetectConfig>(
				session, cmdDest);
		JMSAvroMessageAsyncReceiver<ServiceError, ServiceErrorRecord> errorReceiver =
				new JMSAvroMessageAsyncReceiver<ServiceError, ServiceErrorRecord>(
				session, errDest,
				ServiceErrorRecord.class, ServiceErrorRecord.SCHEMA$);
		ServiceCommandFactory cmdFactory = new PortableServiceCommand.Factory();
		JMSAvroMessageAsyncReceiver<ImageRegionList, ImageRegionListRecord> imageRgnReceiver =
				new JMSAvroMessageAsyncReceiver<ImageRegionList, ImageRegionListRecord>(
				session, imgRgnDest,
				ImageRegionListRecord.class,
				ImageRegionListRecord.SCHEMA$);

		RemoteImageRegionServiceClient<FaceDetectServiceConfig> service =
				new RemoteImageRegionServiceClient<FaceDetectServiceConfig>(
				FaceDetectServiceConfig.class, "imageService", "remoteId",
				cmdSender, configSender, errorReceiver,
				cmdFactory, imageRgnReceiver);

		cmdSender.setAdapter(new EmptyAdapter());
		cmdSender.setDefaultContentType(JMSAvroServiceFacade.COMMAND_MIME_TYPE);
		cmdSender.start();

//        configSender.setAdapter(new PortableFaceDetectServiceConfig.MessageRecordAdapter());
//        configSender.setDefaultContentType(JMSAvroServiceFacade.CONFIG_MIME_TYPE);
//        configSender.start();

		errorReceiver.setAdapter(new EmptyAdapter());
		errorReceiver.start();

		imageRgnReceiver.setAdapter(new EmptyAdapter());
		imageRgnReceiver.start();

		return service;
	}
}
