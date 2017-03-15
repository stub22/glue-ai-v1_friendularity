/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.qpid_broker_wrap;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

// We currently allow the broker launch to happen under superior
// bundle control, so this activator has no responsibilities.
// All the juice is in the QPidBrokerLauncher class, which can be used from any top bundle.
// TODO: Consider making launch configurable from here.

public class QpidBrokerSubsetBundleActivator extends BundleActivatorBase {

    public void start(BundleContext bunCtx) throws Exception {
		super.start(bunCtx);
	}
    public void stop(BundleContext bunCtx) throws Exception {
 		super.stop(bunCtx);
    }

}

/*

http://grepcode.com/snapshot/repo1.maven.org/maven2/org.apache.qpid/qpid-broker-core/0.32

     public static final String QPID_WORK_DIR  = "qpid.work_dir";

Configuration property name for the absolute path to use for the broker home directory.
If not otherwise set, the value for this configuration property defaults to the location set
in the "QPID_HOME" system property if that was set, or remains unset if it was not.

49
50     public static final String QPID_HOME_DIR  = "qpid.home_dir";
51
52     public static final String DEFAULT_INITIAL_CONFIG_NAME = "initial-config.json";
53     public static final String DEFAULT_STORE_TYPE = "JSON";
54     public static final String DEFAULT_CONFIG_NAME_PREFIX = "config";
55     public static final String DEFAULT_LOG_CONFIG_FILE = "etc/log4j.xml";
56     public static final String DEFAULT_INITIAL_CONFIG_LOCATION =
57         BrokerOptions.class.getClassLoader().getResource(DEFAULT_INITIAL_CONFIG_NAME).toExternalForm();
58
59     public static final String MANAGEMENT_MODE_USER_NAME = "mm_admin";
60
61     private static final File FALLBACK_WORK_DIR = new File(System.getProperty("user.dir"), "work");
---------------------------------------------------------
public class BrokerProperties
29 {
30    public static final int  DEFAULT_HEARTBEAT_TIMEOUT_FACTOR = 2;
31    public static final String PROPERTY_HEARTBEAT_TIMEOUT_FACTOR = "qpid.broker_heartbeat_timeout_factor";
32    public static final int HEARTBEAT_TIMEOUT_FACTOR = Integer.getInteger(PROPERTY_HEARTBEAT_TIMEOUT_FACTOR, DEFAULT_HEARTBEAT_TIMEOUT_FACTOR);
33
34    public static final String PROPERTY_DEAD_LETTER_EXCHANGE_SUFFIX = "qpid.broker_dead_letter_exchange_suffix";
35    public static final String PROPERTY_DEAD_LETTER_QUEUE_SUFFIX = "qpid.broker_dead_letter_queue_suffix";
36
37    public static final String PROPERTY_MSG_AUTH = "qpid.broker_msg_auth";
38    public static final String PROPERTY_STATUS_UPDATES = "qpid.broker_status_updates";
39    public static final String PROPERTY_LOCALE = "qpid.broker_locale";
40    public static final String PROPERTY_DEFAULT_SUPPORTED_PROTOCOL_REPLY = "qpid.broker_default_supported_protocol_version_reply";
41    public static final String PROPERTY_DISABLED_FEATURES = "qpid.broker_disabled_features";
42
43    public static final String PROPERTY_MANAGEMENT_RIGHTS_INFER_ALL_ACCESS = "qpid.broker_jmx_method_rights_infer_all_access";
44    public static final String PROPERTY_USE_CUSTOM_RMI_SOCKET_FACTORY = "qpid.broker_jmx_use_custom_rmi_socket_factory";
45
46    public static final String PROPERTY_DEFAULT_SHARED_MESSAGE_GROUP = "qpid.broker_default-shared-message-group";
47
48    public static final String PROPERTY_QPID_HOME = "QPID_HOME";
49    public static final String PROPERTY_QPID_WORK = "QPID_WORK";
50    public static final String PROPERTY_LOG_RECORDS_BUFFER_SIZE = "qpid.broker_log_records_buffer_size";
51    public static final String POSIX_FILE_PERMISSIONS = "qpid.default_posix_file_permissions";
52

 */