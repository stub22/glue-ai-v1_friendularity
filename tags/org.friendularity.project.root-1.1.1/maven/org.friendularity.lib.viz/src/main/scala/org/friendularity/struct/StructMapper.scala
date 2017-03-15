package org.friendularity.struct

/**
  * Created by Owner on 4/13/2016.
  */
// Despite allowing type arguments, this class is still rigid in that it expects all values
// of the StructMapper to conform to the same DV value type.  Thus a single StructMapper may not
// easily output to both Vector3f and Quaternion, for example.    So a large scale logical
// object will often need data inflow from more than one struct.  In outer layers, the values
// from all structs are combined as numeric data via ArrayOfDoubles objects, which are used as the
// currency of the MathStructMappers.

class StructMapper[FK, DV <: DataValue, DE <: DataExpr] extends FactorySupplier[FK, DV] {
	var myFieldExprMap = new scala.collection.immutable.HashMap[FK, DataBinding[DE, DV]]
	//	var myValueFactoryMap = new scala.collection.immutable.HashMap[FK, Factory[DV]]
	// 	def getValueFactoryForField(fk : FK) : Factory[DV] = myValueFactoryMap.apply(fk)
	def bindField(fk : FK, de : DE, dvFactory : Factory[DV]) {
		val bufferDV = dvFactory.make()
		val binding = new DataBinding(de, bufferDV)
		myFieldExprMap -= fk // Remove any old binding for this key.
		myFieldExprMap += (fk -> binding)
		bindFactory(fk, dvFactory)
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
	protected def getFieldExpr(fk : FK ) : Option[DataExpr] = {
		val binding = myFieldExprMap.get(fk)
		if (binding.isDefined) {
			Some(binding.get.myExpr)
		} else {
			None
		}
	}
}