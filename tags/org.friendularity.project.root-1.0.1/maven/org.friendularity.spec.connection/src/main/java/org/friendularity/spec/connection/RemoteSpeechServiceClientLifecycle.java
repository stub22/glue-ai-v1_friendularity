/*
 * Copyright 2013 The JFlux Project (www.jflux.org).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.friendularity.spec.connection;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.robokind.api.messaging.MessageAsyncReceiver;
import org.robokind.api.messaging.MessageSender;
import org.robokind.api.speech.messaging.RemoteSpeechServiceClient;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.robokind.api.speech.SpeechConfig;
import org.robokind.api.speech.SpeechEventList;
import org.robokind.api.speech.SpeechRequest;
import org.robokind.api.speech.SpeechRequestFactory;
import org.robokind.api.messaging.services.ServiceCommand;
import org.robokind.api.messaging.services.ServiceCommandFactory;
import org.robokind.api.messaging.services.ServiceError;

/**
 *
 * @author Major Jacquote II <mjacquote@gmail.com>
 */
public class RemoteSpeechServiceClientLifecycle implements ServiceLifecycle<RemoteSpeechServiceClient> {

    private final static String theCommandSender = "svc_cmd_sender_dep";
    private final static String theConfigSender = "sp_config_sender_dep";
    private final static String theErrorReceiver = "svc_err_receiver_dep";
    private final static String theCommandFactory = "commandFactory";
    private final static String theRequestSender = "request_sender_dep";
    private final static String theEventsReceiver = "sp_event_receiver_dep";
    private final static String theRequestFactory = "requestFactory";
    private final static String remoteIdProps = "sp_remoteClientID_dep";
    private String myLocalServiceId;
    private String myRemoteServiceId;
    private final static String[] theClassNameArray = {
        RemoteSpeechServiceClient.class.getName()
    };
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(theCommandSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theConfigSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theErrorReceiver, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theRequestSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theEventsReceiver, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theCommandFactory, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(theRequestFactory, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(remoteIdProps, RemoteClientPropertySpec.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };

    public RemoteSpeechServiceClientLifecycle() {
    }

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public RemoteSpeechServiceClient createService(Map<String, Object> dependencyMap) {
        RemoteClientPropertySpec rcps = (RemoteClientPropertySpec) dependencyMap.get(remoteIdProps);
        MessageSender<ServiceCommand> commandSender =
                (MessageSender) dependencyMap.get(theCommandSender);
        MessageSender<SpeechConfig> configSender =
                (MessageSender) dependencyMap.get(theConfigSender);
        MessageAsyncReceiver<ServiceError> errorReceiver =
                (MessageAsyncReceiver) dependencyMap.get(theErrorReceiver);
        ServiceCommandFactory commandFactory =
                (ServiceCommandFactory) dependencyMap.get(theCommandFactory);
        MessageSender<SpeechRequest> requestSender =
                (MessageSender) dependencyMap.get(theRequestSender);
        MessageAsyncReceiver<SpeechEventList> eventsReceiver =
                (MessageAsyncReceiver) dependencyMap.get(theEventsReceiver);
        SpeechRequestFactory requestFactory =
                (SpeechRequestFactory) dependencyMap.get(theRequestFactory);

        return new RemoteSpeechServiceClient(
                SpeechConfig.class, rcps.getSpeechServiceId(), rcps.getRemoteId(),
                commandSender, configSender, errorReceiver, commandFactory,
                requestSender, eventsReceiver, requestFactory);
    }

    @Override
    public RemoteSpeechServiceClient handleDependencyChange(RemoteSpeechServiceClient client, String changeType, String dependencyName,
            Object dependency, Map<String, Object> availableDependencies) {
        return null;
    }

    @Override
    public void disposeService(RemoteSpeechServiceClient t, Map<String, Object> map) {

        if (t != null) {
            t.stop();
        }
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
}
