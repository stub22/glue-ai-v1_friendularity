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

package org.friendularity.appro

import org.appdapter.fancy.log.VarargsLogging;

object TestRaizLoad extends VarargsLogging {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//	org.apache.log4j.BasicConfigurator.configure();
		//	org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		//
		//	Appears that currently Akka is automatically initing logging with our log4j.properties.

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestRaizLoad main().START");
	}
}
