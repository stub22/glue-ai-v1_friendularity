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

package org.friendularity.netcli.bigtest

/**
  * Created by Stub22 on 8/11/2016.
  *
  * The "netcli" pkg hierarchy is used by network clients, *outside* of any simulator, robot,
  * or vworld process.   It generally should not be OSGi-exported by bundles that are deployed *inside*
  * such processes.  It may be used in unit tests and load tests to exercise such server code.
  *
  * Network = any combination of HTTP-REST, AMQP, and Akka-Remote-TCP (via netty).
  */
class TestNetworkClients {

}
