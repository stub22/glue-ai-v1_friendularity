
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

package org.friendularity.infra.jfdep

import java.util.{HashMap => JavaHashMap, List => JavaList, Map => JavaMap}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.jflux.api.service.{ServiceDependency, ServiceLifecycle}

import scala.collection.JavaConverters._

trait DepSpecMaker { 
	protected def makeStaticUnaryDepSpec(depName : String, clz : Class[_]) : ServiceDependency = {
		makeStaticUnaryDepSpec(depName, clz.getName)
	}
	protected def makeStaticUnaryDepSpec(depName : String, clzName : String) : ServiceDependency = {
		val card = ServiceDependency.Cardinality.MANDATORY_UNARY
		val upStrat = ServiceDependency.UpdateStrategy.STATIC
		val unusedExoticUsagePropsMap_forFutureUse_canBeNull = new JavaHashMap[String,String]()
		new ServiceDependency(depName, clzName, card, upStrat, unusedExoticUsagePropsMap_forFutureUse_canBeNull)
	}

}
import org.jflux.api.registry.basic.BasicDescriptor
import org.jflux.api.service.binding.ServiceBinding
import org.jflux.api.service.binding.ServiceBinding.BindingStrategy;

trait BindingMaker {
	protected def appendEagerBinding(bindingMap : JavaHashMap[String,ServiceBinding], 
									   dep : ServiceDependency, tagKey: String, tagVal : String) : ServiceBinding = { 
		val propsMap = new JavaHashMap[String,String]()
		propsMap.put(tagKey, tagVal)
		val clzName = dep.getDependencyClassName
		val bindingDesc = new BasicDescriptor(clzName, propsMap);
		val binding = new ServiceBinding(dep, bindingDesc, BindingStrategy.EAGER);
		bindingMap.put(binding.getDependencyName, binding)
		binding
	}
}

// Weak -> Without further overrides, can only process createService - cannot do dispose (warning) or change (error).
trait WeakLifecycle[ServType] extends ServiceLifecycle[ServType] with VarargsLogging with DepSpecMaker { 
	
	override def disposeService(serv: ServType, availDeps : java.util.Map[String, java.lang.Object]): Unit = {
		warn1("WeakLifecycle.disposeService - default impl called for service type ", getClass.getName)
	}

	override def handleDependencyChange(serv: ServType, changeType: String, dependencyName: String, dep: AnyRef, 
				availDeps: JavaMap[String, Object]): ServType = {
		// This is an explicitly messaged equivalent to ???	
		error3("WeakLifecycle.handleDependencyChange default impl called on serv-class {} with changeType {}, depName {}, will throw",
					getClass, changeType, dependencyName)
		throw new RuntimeException("WeakLifecycle.handleDependencyChange default impl - must override in " + getClass.getName)
	}
}
// These traits take care of some of the vanilla aspects of "getServiceClassName" stuff for the most common cases.
// 
// TODO : Consider creating a more general ClazzyLifecycle allowing for a list of service interfraces
trait OneOutLifecycle[ServType] extends ServiceLifecycle[ServType] {
	
	protected def getOutClz : Class[_ <: ServType] // Class must define - see OneOutClazzyLifecycle
	
	lazy val srvClzNames :Array[String] = Array(getOutClz.getName)
	
	override def getServiceClassNames(): Array[String] = srvClzNames
}
// Fancy --> uses scala types
trait FancyOneOutWeakLifecycle[ServType] extends WeakLifecycle[ServType] with OneOutLifecycle[ServType] {
	
	protected def makeDepSpecsSL : List[ServiceDependency]
	
	lazy val myDepSpecsJL : JavaList[ServiceDependency] = makeDepSpecsSL.asJava	

	override def getDependencySpecs(): JavaList[ServiceDependency] = myDepSpecsJL
	

}
abstract class OneOutClazzyLifecycle[ServType](srvClz : Class[_ <: ServType]) extends FancyOneOutWeakLifecycle[ServType] {
	override protected def getOutClz : Class[_ <: ServType] = srvClz
}

trait OneInOneOutLCycle[InType,OutType] extends ServiceLifecycle[OutType] with VarargsLogging with DepSpecMaker {
}
abstract class OneInOneOutLCycleImpl[InType,OutType](inClz : Class[_ <: InType], outClz : Class[_ <: OutType]) extends OneInOneOutLCycle[InType, OutType] {
}