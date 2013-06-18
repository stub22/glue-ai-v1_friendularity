package org.friendularity.bundle.demo.convo.response;

import org.apache.avro.generic.IndexedRecord;
import org.jflux.api.core.Listener;
import org.robokind.api.common.utils.TimeUtils;
import org.robokind.api.speechrec.SpeechRecEvent;
import org.robokind.avrogen.speech.SpeechRequestRecord;
import org.robokind.avrogen.speechrec.SpeechRecEventListRecord;

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
        SpeechRecEventListRecord eventList =
                (SpeechRecEventListRecord)t;
        SpeechRequestRecord.Builder builder = SpeechRequestRecord.newBuilder();

        SpeechRecEvent event = eventList.getSpeechRecEvents().get(0);
        builder.setPhrase(event.getRecognizedText());
        builder.setRequestSourceId(event.getRecognizerId());
        builder.setTimestampMillisecUTC(TimeUtils.now());
        builder.setSpeechServiceId("speechService");

        SpeechRequestRecord request = builder.build();
        myConnector.send(request);
    }
    
}
