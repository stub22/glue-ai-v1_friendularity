/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.respire

import org.friendularity.struct.{ArrayOfDoubles, Factory}

/**
 * @author Stu B. <www.texpedient.com>
 */

trait TimeIndex {
	
}
trait TimeIndexRange {
	
}
trait ValueRangeMetadata {
	// Any description of a point of view on the data in some range (and possibly all of) the series.
	// Can be used from both read + write sides of the series.
}
abstract class ValuesForTimeRange[+VT] {
	def getValue(ti : TimeIndex) : VT
}
trait SeriesReader[SeriesKey] {
	
	// Synchronously poll-query any continuous range of data.  If this operation will be long or expensive,
	// the client may help out by calling "prepareRange".  The return value may be freshly allocated or
	// come from a pool known to the reader.
	// We allow types of the series data to vary over RVT, keyed by sk.
	// User must know how to query via
	// The approach used in our Struct classes is in some ways simpler - always supplying a value-target,
	// because that way dispenses with output type/factory/constructor concerns.  It is generally permissible
	// to use that explicit target form to shortcut this return type, but this value-returning form has
	// some utility as well.
	def readRange[RVT](sk : SeriesKey, tir : TimeIndexRange) : ValuesForTimeRange[RVT]
	
	// Begins any available asynchronous processes for helping improve the results of future requests to readRange.
	def prepareRange(sk: SeriesKey,  tir : TimeIndexRange, vrm : ValueRangeMetadata) : Unit = { }
	
	def readAOD(sk : SeriesKey, tir : TimeIndexRange) : ValuesForTimeRange[ArrayOfDoubles] = {
		readRange(sk, tir)
	}
}
trait SeriesWriter[SeriesKey] {
	// writeInstant can be called when raw data is received, or a newly-computed result is cached
	def writeInstant[WVT](sk : SeriesKey, tidx : TimeIndex, instVal : WVT) : Unit = { }
	
	// This can be called to establish a rule/equation relationship between the sk-series and some other data
	// (which often involves some other series accessed by reference).
	def writePattern[WPT](sk : SeriesKey, pattern : WPT, vrm : ValueRangeMetadata) : Unit = { }
}
// This trait is implemented by callbacks, and called on a thread either known by or provided to the application.
trait SeriesListener[SeriesKey] {
	// Each value update to series(sk), at a particular time is delivered in v.
	// This method is polymporphic in the value type.
	// We presume that listeners are able to map their own data over using mapValue().
	def notifyValue[NVT](sk : SeriesKey, tidx : TimeIndex, v : NVT)
	
	// Mapping happens during each notify cycle.
	//		FROM:	The value type supplied by the outer caller to notify().
	//		TO:		The value type registered with the outer Notifier binding.
	// To provide conversion, override this trivial casting impl.
	def mapValue[SrcT,TgtT](src : SrcT) : TgtT = src.asInstanceOf[TgtT]
}

class NotifierBinding[SeriesKey, VT](myKey : SeriesKey, val myLstnr : SeriesListener[SeriesKey]) {
	type	MyValueType = VT
	
	def doNotify(tidx : TimeIndex, v : MyValueType) {
		myLstnr.notifyValue(myKey, tidx, v)
	}
	// type variable remembers the data binding type for this series
}
trait SeriesNotifier[SeriesKey] {

/*
 * We keep the notifier concept separate from the reader.
 */	
	val myBinds = new scala.collection.mutable.HashMap[SeriesKey, scala.collection.mutable.Set[NotifierBinding[SeriesKey, _]]]
	
	def registerListener[RVT](sk : SeriesKey, tir : TimeIndexRange, serListener : SeriesListener[SeriesKey]) : Unit = {
		val listrSet = myBinds.getOrElseUpdate(sk, new scala.collection.mutable.HashSet[NotifierBinding[SeriesKey, _]])
		val nb = new NotifierBinding(sk, serListener)
		listrSet.add(nb)
	}

	def getBindingSet(sk : SeriesKey) : Traversable[NotifierBinding[SeriesKey, _]] = {
		myBinds.getOrElse(sk, new scala.collection.immutable.HashSet[NotifierBinding[SeriesKey, _]]())
	}
		

	def doNotify[VT](sk : SeriesKey, tidx : TimeIndex, v : VT) {
		val notiGroup = myBinds.getOrElse(sk, Nil);
		notiGroup.foreach (nb => {
			// val nl : List[nb.MyValueType] = Nil
			// 	val mappedValueCast : nb.MyValueType = v.asInstanceOf[nb.MyValueType] 
			// Each listener must take responsiblity for converting data into the type that it knows how to
			// listen for.
			val mappedValueFunc : nb.MyValueType = nb.myLstnr.mapValue(v)
			nb.doNotify(tidx, mappedValueFunc)
			// if (n.myValueType.)
			
		})
			
	}
}



class AODSL[SeriesKey] extends SeriesListener[SeriesKey] {
		
	override def notifyValue[ArrayOfDoubles](sk : SeriesKey, tidx : TimeIndex, v : ArrayOfDoubles) {
		
	}
}

case class OtherIndexedValues[VT]( factory : Factory[VT]) extends  ValuesForTimeRange[VT] {
	override def getValue(ti : TimeIndex) : VT = factory.makeOne
}


case class TestIndexedValues extends  ValuesForTimeRange[ArrayOfDoubles] {
	override def getValue(ti : TimeIndex) : ArrayOfDoubles = new ArrayOfDoubles(Array[Double](3.14))
}

import org.appdapter.core.name.{Ident, FreeIdent};

class TestWriter extends SeriesWriter[Ident]
/*
class TestReader extends SeriesReader[Ident] {
	override def readRange[RVT](sk : Ident, tir : TimeIndexRange) : ValuesForTimeRange[RVT] = {
		val tiv : TestIndexedValues = new TestIndexedValues
		
		val oiv = new OtherIndexedValues
		// Why is this cast necessary?  
		val vftr : ValuesForTimeRange[RVT] = new ValuesForTimeRange[RVT]//  tiv // .asInstanceOf[ValuesForTimeRange[ArrayOfDoubles]]
		vftr
	}
}
*/

class TestTimeIndex extends TimeIndex

class TestTimeIndexRange extends TimeIndexRange

object SeriesPortal extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		val tw = new TestWriter
		// val tr = new TestReader
		
		val someKey = new FreeIdent("ok#HeyThere")
		val someTimeIndx = new TestTimeIndex
		val someTimeRange = new TestTimeIndexRange
	//	val readOut : ValuesForTimeRange[ArrayOfDoubles] = tr.readRange(someKey, someTimeRange)
	//	getLogger().info("Read: {}", readOut)
		
		val vftr : ValuesForTimeRange[ArrayOfDoubles] = new TestIndexedValues()

	}
}
// A TimeSeries *may* be thought of as an RDF graph, which accepts or produces ThingActions.
// Such a binding is useful for relatively slowly-changing series.
