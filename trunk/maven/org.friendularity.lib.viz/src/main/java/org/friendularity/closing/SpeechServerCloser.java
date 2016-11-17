package org.friendularity.closing;

import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.url.URLSyntaxException;
import org.jflux.api.core.util.EmptyAdapter;
import org.jflux.api.messaging.rk.services.ServiceCommand;
import org.jflux.impl.messaging.rk.JMSAvroMessageSender;
import org.jflux.impl.messaging.rk.JMSAvroServiceFacade;
import org.jflux.impl.messaging.rk.ServiceCommandRecord;

import java.net.URISyntaxException;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;

/**
 * Currently capable of cleanly closing the SAPI speech or Acapela speech server.
 *
 * @author ben
 * @since 11/16/2016.
 */
public class SpeechServerCloser {

	private final String myBrokerIPAddress;

	public SpeechServerCloser() {
		this("127.0.0.1");
	}

	public SpeechServerCloser(final String brokerIPAddress) {
		myBrokerIPAddress = brokerIPAddress;
	}


	public void close() throws JMSException, URISyntaxException {

		Connection connection = buildConnection();
		Session session = connection.createSession(false, Session.CLIENT_ACKNOWLEDGE);
		try {
			connection.start();
			try {
				Destination cmdDest = new AMQQueue("speechCommand; {create: always, node: {type: queue}}");

				JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord> cmdSender =
						new JMSAvroMessageSender(session, cmdDest);

				cmdSender.setAdapter(new EmptyAdapter());
				cmdSender.setDefaultContentType(JMSAvroServiceFacade.COMMAND_MIME_TYPE);

				cmdSender.start();

				try {
					sendStopCommandRecord(cmdSender);

				} finally {
					cmdSender.stop();

				}
			} finally {
				connection.stop();

			}
		} finally {
			session.close();

		}


	}

	private Connection buildConnection() throws URLSyntaxException, JMSException {
		String brokerTCP_Addr = myBrokerIPAddress + ":5672";
		String brokerAMQP_URL = "amqp://admin:admin@clientid/test?brokerlist='tcp://" + brokerTCP_Addr + "'";
		ConnectionFactory cf = new AMQConnectionFactory(brokerAMQP_URL);
		return cf.createConnection();
	}

	private void sendStopCommandRecord(JMSAvroMessageSender<ServiceCommand, ServiceCommandRecord> cmdSender) {
		ServiceCommandRecord serviceCommandRecord = new ServiceCommandRecord();
		serviceCommandRecord.setCommand("stop");
		serviceCommandRecord.setTimestampMillisecUTC(System.currentTimeMillis());
		serviceCommandRecord.setSourceId("source");
		serviceCommandRecord.setDestinationId("destination");
		cmdSender.sendMessage(serviceCommandRecord, "application/service-command");
	}

	@Override
	public String toString() {
		return "SpeechServerCloser{" +
				"brokerIPAddress='" + myBrokerIPAddress + '\'' +
				'}';
	}
}