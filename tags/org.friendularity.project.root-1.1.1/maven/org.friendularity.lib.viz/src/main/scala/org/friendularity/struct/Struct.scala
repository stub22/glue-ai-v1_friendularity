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

// A Struct is a key-value read+write interface to some state, suitable for aggregating
// with other DataValues.
// The readField and writeField methods read/write a struct-private copy of the data, separate from dVal.
// This trait does not specify whether the fields must exist internally before being written,
// or how fields might be made if they are not available, or whether an error should occur instead.
// Those issues are addressed by subtypes of this Trait.
// This approach implies that all exposed fields are readable/writable using type FieldVal.
// This policy is somewhat restrictive.  Alternatives require an additional layer of indirection.
// 
// Note that as of 2014-06-01, DataValue is still just an unused marker trait, should perhaps be an annotation instead of trait
trait Struct[FieldKey, FieldVal] extends DataValue {

	def writeField(fk : FieldKey, dVal : FieldVal) : Unit
	// We avoid assuming it is easy to create a FieldVal of appropriate type, by using FieldVal arg as 
	// target for reading into, rather than supplying a return value from the method
	// This approach also fits our larger constant-memory ring-buffer pattern.
	def readField(fk : FieldKey, dVal : FieldVal) : Unit
}



trait FactoriedStruct[FK, FV] extends Struct[FK, FV] {
	var myFieldValMap : Map[FK, FV] = new scala.collection.immutable.HashMap[FK, FV]

	protected def makeFieldVal(fk : FK) : FV = getFactory(fk).make()
	protected def getFactory(fk : FK) : Factory[FV]

	override def writeField(fk : FK, dVal : FV) {
		val tgtFV : FV =
			if (myFieldValMap.contains(fk)) {
				myFieldValMap.apply(fk)
			} else {
				val tfv = makeFieldVal(fk)
				myFieldValMap += (fk -> tfv)
				tfv
			}
		getFactory(fk).shallowCopyContents(dVal, tgtFV)
	}
	override def readField(fk : FK, tgtFV : FV) {
		val srcFV = myFieldValMap.apply(fk)
		getFactory(fk).shallowCopyContents(srcFV, tgtFV)
	}

}
class IndirectlyFactoriedStruct[FK, FV](myFactorySupplier : FactorySupplier[FK,FV]) extends FactoriedStruct[FK, FV] {
	override protected def getFactory(fk : FK) : Factory[FV] = myFactorySupplier.getValueFactoryForField(fk)
}

// Uses a fieldFactory to copy values as required, and *by default* to make new values.
class DirectlyFactoriedStruct[FK, FV](val myFieldFactory : Factory[FV]) extends FactoriedStruct[FK, FV] {
	override protected def getFactory(fk : FK) : Factory[FV] = myFieldFactory

	override def toString(): String = "BasicStruct[" + myFieldValMap + "]"
}


// The myMapper StructMapper is a somewhat tricky object, intended for actor-managed or
// single-threaded use (contains "unsafe" buffers).  These handles should only be used
// within that same actor/thread ctx.
abstract class MappedStructHandle[FK, DV <: DataValue, DE <: DataExpr](val myMapper : StructMapper[FK, DV, DE]) {
	// We make a general purpose struct to store an internal copy of the data.
	// The mapper arg here is used by the struct *only* as the supplier of value factories for the fields.
	// Assumptions about what expressions are bound to the field are independent, although in this handle's design,
	// the same mapper instance is used for both purposes.
	lazy private val myIntrnStruct = new IndirectlyFactoriedStruct[FK, DV](myMapper)
	
	def getDataSource : DataSource[DE, DV]
	
	def updateSourcedFields() {
		val dataSource = getDataSource
		myMapper.mapSourceDataToStruct(dataSource, myIntrnStruct)
	}
	def readCachedFieldValue(fk : FK, tgtDV : DV) {
		myIntrnStruct.readField(fk, tgtDV)
	}

	def allocateFieldVal(fk : FK) : DV =  myMapper.makeFieldVal(fk)
	
	def getResultFieldCopy(fk : FK) : DV = {
		val tgtFV = allocateFieldVal(fk)
		readCachedFieldValue(fk, tgtFV)
		tgtFV
	}
	
}
