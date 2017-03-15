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

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}
import org.apache.jena.riot.RDFFormat
import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Stub22 on 8/11/2016.
  */

trait JenaModelReader extends VarargsLogging {
	def readModelFromTurtleTxt(turtleTxt : String, flags_opt: Option[AnyRef]) : JenaModel = {
		// val modelReader = new StringReader(modelTurtleTxt)
		// Jena docs recommend reading from bytes rather than higher level Java chars.
		val modelBytes = turtleTxt.getBytes(StandardCharsets.UTF_8)

		val modelByteStream = new ByteArrayInputStream(modelBytes);
		val model = JenaModelFactory.createDefaultModel() ;
		val baseURI_orNull : String = null
		val lang : String = RDFFormat.TURTLE.getLang.getName //  "TURTLE"
		model.read(modelByteStream, baseURI_orNull, lang)
		debug1("After read, model size is (at least) {} stmts", model.size() : java.lang.Long)
		trace1("Model contentDump:\n{}", model)
		model
	}
}

