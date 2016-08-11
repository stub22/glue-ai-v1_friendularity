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


package org.friendularity.thact

import java.util.Random

import com.hp.hpl.jena.rdf.model.Model

import java.lang.{Long => JLong}
import java.util.{ArrayList => JArrayList, List => JList, Random, Set => JSet}
import com.hp.hpl.jena.rdf.model.{Literal, Model => JenaModel, ModelFactory => JenaModelFactory, ResIterator, Resource}

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.api.thing.ThingActionSpec

/**
  * Created by Stub22 on 8/11/2016.
  */

trait ThingActTurtleEncoder extends VarargsLogging {
	lazy val myRandomizer: Random = new Random

	def encodeAsTurtleMsg(actSpec : ThingActionSpec) : String = {
		val ftmw = new FancyThingModelWriter
		val specModelWithPrefixes : JenaModel  = ftmw.writeTASpecAndPrefixesToNewModel(actSpec, myRandomizer)

		val turtleTriplesString : String = ftmw.serializeSpecModelToTurtleString(specModelWithPrefixes)
		info2("Serialized turtle message FROM model of size {} triples TO string of length {} chars", specModelWithPrefixes.size() : JLong, turtleTriplesString.length : Integer)
		debug1("Debug-Dumping encoded turtle message:\n {}", turtleTriplesString)
		turtleTriplesString
	}
}
