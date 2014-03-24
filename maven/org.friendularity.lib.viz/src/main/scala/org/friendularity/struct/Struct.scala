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

// A Struct is just an interface to some state that can be read/written using the value type FieldVal.
// The readField and writeField methods read/write a struct-private copy of the data, separate from dVal.
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

// An updatable set of bindings for keyed fields, which can be used to map from any compatible data source
// into any compatible struct.  
class StructMapper[FK, DV <: DataValue, DE <: DataExpr] {
	var myFieldExprMap = new scala.collection.immutable.HashMap[FK, DataBinding[DE, DV]]

	def bindField(fk : FK, de : DE, bufferDV : DV) {
		val binding = new DataBinding(de, bufferDV)
		myFieldExprMap += (fk -> binding)
	}
	// Updates the fields of a given struct, using mappings embedded in our DataBindings for each field,
	// and source data from the given DataSource.  
	// For each "field expression" in this mapper, read current value for that field from the given DataSource, using the field's
	// DataBinding (which contains some hidden DataExpr that we don't see directly, as well as an unsafe buffer we don't see), 
	// and write it into the given struct.
	// Again, we avoid assuming that it is easy to create new values, and instead we write into exsiting values.
	def mapSourceDataToStruct( ds : DataSource[DE, DV], s : Struct[FK, DV]) : Unit = {
		for ((fk,b) <- myFieldExprMap) {
			// Read data from the DataSource, using the expression embedded in the binding
			b.readSourceField(ds) // Now the data is held in the private buffer of the binding
			// Copy from the binding's buffer into the target struct.
			s.writeField(fk, b.myBufferDV)
		}
	}
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
