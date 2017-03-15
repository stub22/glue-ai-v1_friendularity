package org.friendularity.bundle.demo.convo.response;

import org.jflux.api.common.rk.utils.TimeUtils;
import org.jflux.api.core.Listener;
import org.mechio.api.speechrec.SpeechRecEvent;
import org.mechio.impl.speech.SpeechRequestRecord;
import org.mechio.impl.speechrec.SpeechRecEventListRecord;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class Responder implements Listener<SpeechRecEventListRecord> {
    private DualQpidConnector myConnector;
    
    public Responder(DualQpidConnector connector) {
        myConnector = connector;
    }
    
    @Override
    public void handleEvent(SpeechRecEventListRecord t) {
        SpeechRequestRecord.Builder builder = SpeechRequestRecord.newBuilder();

        SpeechRecEvent event = t.getSpeechRecEvents().get(0);
        builder.setPhrase(event.getRecognizedText());
        builder.setRequestSourceId(event.getRecognizerId());
        builder.setTimestampMillisecUTC(TimeUtils.now());
        builder.setSpeechServiceId("speechService");

        SpeechRequestRecord request = builder.build();
        myConnector.send(request);
    }
    
}
