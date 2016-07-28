package org.friendularity.bundle.headless.speech;

import java.util.Properties;
import java.util.logging.Logger;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
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
import org.mechio.impl.speech.SpeechConfigRecord;
import org.mechio.impl.speech.SpeechEventListRecord;
import org.mechio.impl.speech.SpeechRequestRecord;
import org.mechio.impl.speech.PortableSpeechRequest;

public class Activator extends BundleActivatorBase {

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		// We want these services to be activated by config.
		getLogger().warn("o.f.b.headless.speech Activator.start() no longer does any service launching.");

	}


    @Override
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
    }
}
