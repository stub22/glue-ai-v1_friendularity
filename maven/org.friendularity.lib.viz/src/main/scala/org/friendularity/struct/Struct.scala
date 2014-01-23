/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.struct
import org.cogchar.bind.symja.MathGate;
import org.appdapter.core.name.Ident;
import org.friendularity.api.struct.Maker;
/**
 * @author Stu B. <www.texpedient.com>
 */

trait DataExpr {
	// Marker trait that we might not really need
}
trait DataValue {
	// Marker trait that we might not really need
}
/* Interesting:
 org/friendularity/struct/Struct.scala:150: error: type mismatch;
 found   : Array[Elem with Object]
 required: Array[Elem]
 Note: Elem with Object <: Elem, but class Array is invariant in type T.
 You may wish to investigate a wildcard type such as `_ <: Elem`. (SLS 3.2.10)
 val myElems : Array[Elem] = myElemFactory.makeArray(mySize)
 http://stackoverflow.com/questions/10000126/re-using-java-generic-collections-in-scala-without-trait-object	
 */

// If this were a trait instead of abstract class, then it would not be properly extensible 
// from Java, because it contains method impls.
abstract class Factory[V] extends Maker[V] {
	// If V represents any kind of an array, then make() must know how big that array is.
	def make() : V = makeOne()
	def makeArray(size : Int) : Array[V] 

}
trait DataSource[DE <: DataExpr, DV <: DataValue] {
	def read(expr : DE, dataValue : DV)
}
class MathGateExpr(val myExprString : String) extends DataExpr {
}
// Scala's  Array[Double] compiles to java:  double[]  
class ArrayOfDoubles(val myVals : Array[Double]) extends DataValue  {
	override def toString(): String = "AOD[" + myVals.mkString(", ") + "]"
}
class AODFactory(val myArraySize : Int) extends Factory[ArrayOfDoubles] {
	override def makeOne() : ArrayOfDoubles = {
		val a = new Array[Double](myArraySize)
		new ArrayOfDoubles(a)
	}
	override def makeArray(size : Int) : Array[ArrayOfDoubles] = {
		new Array[ArrayOfDoubles](size)
	}
	override def shallowCopyContents(source : ArrayOfDoubles, target: ArrayOfDoubles) {
		source.myVals.copyToArray(target.myVals)
	}
}
class OneDouble(val myVal : Double) extends DataValue {
}
class MathGateDoublesSource(val myMG : MathGate) extends DataSource[MathGateExpr, ArrayOfDoubles] {
	override def read(expr : MathGateExpr, dataValue : ArrayOfDoubles) = {
		myMG.parseAndEvalExprToDoubleVec(expr.myExprString, dataValue.myVals);
	}
}
// A Struct is just an interface to some state that can be read/written using FieldVal.
// These methods read/write a private copy of the data, separate from dVal.
// This trait does not specify whether the fields must exist internally before being written,
// or how fields might be made if they are not available, or whether an error should occur instead.
// Those issues are addressed by subtypes of this Trait.
// This approach implies that all exposed fields are readable/writable using type FieldVal.
// This policy is somewhat restrictive.  Alternatives require an additional layer of indirection.
trait Struct[FieldKey, FieldVal] extends DataValue {

	def writeField(fk : FieldKey, dVal : FieldVal)
	// We avoid assuming it is easy to create a FieldVal of appropriate type, by using FieldVal arg as 
	// target for reading, rather than as a return value.
	// This approach also fits our larger constant-memory ring-buffer pattern.
	def readField(fk : FieldKey, dVal : FieldVal)
}
// The buffer is an unsafe temp-buffer for us to use in bridging between data-source and (all) structs
class Binding[DE <: DataExpr, BDV <: DataValue](val myExpr : DE, val myBufferDV : BDV) {
	def readSourceField(ds : DataSource[DE, BDV]) : Unit = {
		// Un-threadsafe use of temp buffer val
		ds.read(myExpr, myBufferDV)
	}
}
class StructMapper[FK, DV <: DataValue, DE <: DataExpr] {
	var myFieldExprMap = new scala.collection.immutable.HashMap[FK, Binding[DE, DV]]

	def bindField(fk : FK, de : DE, bufferDV : DV) {
		val binding = new Binding(de, bufferDV)
		myFieldExprMap += (fk -> binding)
	}
	// Again, we avoid assuming that it is easy to create new values, and instead we write into exsiting values.
	def mapSourceDataToStruct( ds : DataSource[DE, DV], s : Struct[FK, DV]) : Unit = {
		for ((fk,b) <- myFieldExprMap) {
			b.readSourceField(ds)
			s.writeField(fk, b.myBufferDV)
		}
	}
}

trait FieldMaker[FK, DV <: DataValue] {
	// Unused
	def makeField(fieldKey : FK) : DV
}
// Uses a fieldFactory to copy values as required, and *by default* to make new values.
class BasicStruct[FK, FV](val myFieldFactory : Factory[FV]) extends Struct[FK, FV] {
	var myFieldValMap : Map[FK, FV] = new scala.collection.immutable.HashMap[FK, FV]
	override def writeField(fk : FK, dVal : FV) {
		val tgtFV : FV = 
			if (myFieldValMap.contains(fk)) {
				myFieldValMap.apply(fk)
			} else {
				val tfv = makeFieldVal(fk)
				myFieldValMap += (fk -> tfv)
				tfv
			}
		myFieldFactory.shallowCopyContents(dVal, tgtFV)
	}
	override def readField(fk : FK, tgtFV : FV) {
		val srcFV = myFieldValMap.apply(fk)
		myFieldFactory.shallowCopyContents(srcFV, tgtFV)
	}
	protected def makeFieldVal(fk : FK) : FV = {
		// This default impl uses the fieldFactory
		// Override to make() a correct variation based on the fieldKey.
		println("Using factory to make default field config for " + fk)
		myFieldFactory.make()
	}
	override def toString(): String = "BasicStruct[" + myFieldValMap + "]"	
}

object RigidBodyFieldNames {
	val	(pos, dir) = ("pos", "dir")
	val aodf = new	AODFactory(1)
	val pos3Factory = new AODFactory(3)
	val dir4Factory = new AODFactory(4)
}
class RigidBodyStruct extends BasicStruct[String, ArrayOfDoubles](RigidBodyFieldNames.aodf) {
	override protected def makeFieldVal(fk : String) : ArrayOfDoubles = {
		fk match {
			case RigidBodyFieldNames.pos => RigidBodyFieldNames.pos3Factory.make()
			case RigidBodyFieldNames.dir => RigidBodyFieldNames.dir4Factory.make()
		}
	}
}
// class SnapTimes extends Struct[FK, FV] {	
// }
class SnapStruct extends BasicStruct[String, ArrayOfDoubles](RigidBodyFieldNames.aodf) {
}
class RingBuf[Elem](val mySize : Int, val myElemFactory : Factory[Elem]) {
	val myElems : Array[Elem] = myElemFactory.makeArray(mySize)
	var	myCurrentIndex = 0
	// Do we like having empty elements initially? 
	for (idx <- 0 to mySize-1) {
		myElems(idx) = myElemFactory.make()
	}
	def getCurrent() : Elem = myElems(myCurrentIndex)
	
	def advance() {
		myCurrentIndex = (1 + myCurrentIndex) % mySize
	}
	// 0 = current
	def getPrevious(howManyBack : Int) : Elem = {
		// Java can return negative remainders - this formula corrects to always give a positive modulus.
		val prevIndex = (((myCurrentIndex - howManyBack) % mySize) + mySize) % mySize;
		myElems(prevIndex)
	}
	// Considering this, where howMany <= mySize
	// Order is reversed: 
	// result[0] = current, result[1] = previous.
	
	
	// def getCurrent
}
// class AgnosticStruct 
/*
 class ClassyIdentStruct extends Struct[Ident] {
	
 }
 */