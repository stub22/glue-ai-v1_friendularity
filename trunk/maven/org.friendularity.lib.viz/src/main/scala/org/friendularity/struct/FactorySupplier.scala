package org.friendularity.struct

/**
  * Created by Owner on 4/13/2016.
  */
// An updatable set of bindings for keyed fields, which can be used to map from any compatible
// data source into any compatible struct.
trait FactorySupplier[FK, FV] {
	var myValueFactoryMap = new scala.collection.immutable.HashMap[FK, Factory[FV]]
	def getValueFactoryForField(fk : FK) : Factory[FV] = myValueFactoryMap.apply(fk)
	// alt:  apply, applyOrElse(fk, defaultFactory)
	def makeFieldVal(fk : FK) : FV = {
		val factory = getValueFactoryForField(fk)
		// This default impl uses the fieldFactory
		// Override to make() a correct variation based on the fieldKey.
		println("Using factory to make default field config for " + fk)
		factory.make()
	}
	def bindFactory(fk : FK, dvFactory : Factory[FV]) {
		myValueFactoryMap += (fk -> dvFactory)
	}

}