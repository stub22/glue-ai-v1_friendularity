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


import org.jflux.api.service.{ServiceDependency, ServiceLifecycle};
// import org.mechio.api.speech.SpeechService;

import java.util.{HashMap => JavaHashMap, List => JavaList, Map => JavaMap}

import com.hp.hpl.jena
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.blob.entry.EntryHost

import scala.collection.JavaConverters._

class ApproRaizLifecycle extends ServiceLifecycle[ApproRaiz] with VarargsLogging with DepSpecMaker {
	// These names are used as *local* depdendency keys within this lifecycle.
	// These are *not* the keys for properties on the services we consume.
	// See ApproRaizLifeLauncher below for those keys.
	private val DEP_NAME_recipeModel = "arz-dep-model-recipes"
	private val DEP_NAME_settingsModel = "arz-dep-model-settings"
	private val DEP_NAME_statusOutModel = "arz-dep-model-status"

	private val DEP_NAME_legacyConfEntryHost = "arz-dep-ehost-legacyConf"
	private val DEP_NAME_recipeNames = "arz-dep-recipe-names"

	// Class names
	val jmodelClzName = classOf[JenaModel].getName
	val ehostClzName = classOf[EntryHost].getName
	val lsRecNamesClzName = classOf[ApproRaizRecipeNames].getName
	val arClzName = classOf[ApproRaiz].getName

	// The actual dependency objects, which are needed when a user wants to *bind* this lifecycle to some
	// particular useful service property filters.  (For those, see ApproRaizLifeLauncher below).
	lazy val myDepSpec_Recipes : ServiceDependency = makeStaticUnaryDepSpec(DEP_NAME_recipeModel, jmodelClzName)
	lazy val myDepSpec_Settings : ServiceDependency = makeStaticUnaryDepSpec(DEP_NAME_settingsModel, jmodelClzName)
	lazy val myDepSpec_StatusOut : ServiceDependency = makeStaticUnaryDepSpec(DEP_NAME_statusOutModel, jmodelClzName)

	lazy val myDepSpec_EHost_LegCnf : ServiceDependency = makeStaticUnaryDepSpec(DEP_NAME_legacyConfEntryHost, ehostClzName)
	lazy val myDepSpec_RecNames : ServiceDependency = makeStaticUnaryDepSpec(DEP_NAME_recipeNames, lsRecNamesClzName)

	lazy val myDepSpecs = List[ServiceDependency](myDepSpec_Settings, myDepSpec_Recipes, myDepSpec_StatusOut, 
					myDepSpec_EHost_LegCnf, myDepSpec_RecNames)
	lazy val myDepSpecsJL : JavaList[ServiceDependency] = myDepSpecs.asJava
	override def getDependencySpecs(): JavaList[ServiceDependency] = {
		myDepSpecsJL
	}
	override def getServiceClassNames(): Array[String] = Array(arClzName)

	override def createService(availDeps: java.util.Map[String, java.lang.Object]) : ApproRaiz = {
		// Collect the 5 dependency objects...
		val settingsModel = availDeps.get(DEP_NAME_settingsModel).asInstanceOf[JenaModel]
		val recipeModel = availDeps.get(DEP_NAME_recipeModel).asInstanceOf[JenaModel]
		val outStatusModel = availDeps.get(DEP_NAME_statusOutModel).asInstanceOf[JenaModel]

		val legacyCnfEHost = availDeps.get(DEP_NAME_legacyConfEntryHost).asInstanceOf[EntryHost]
		val recipeNames = availDeps.get(DEP_NAME_recipeNames).asInstanceOf[ApproRaizRecipeNames]
		// and put them all in a nice tidy ApproRaizCtx.
		val lsci = new ApproRaizCtxImpl(settingsModel, recipeModel, outStatusModel, legacyCnfEHost, recipeNames)
		info3("ApproRaizLifecycle.createService made context {}, recipe-modelSize={}, settings-modelSize={}",
				lsci, recipeModel.size: java.lang.Long, settingsModel.size : java.lang.Long)
		
		// Now construct the actual service-impl.
		val ari = new ApproRaizImpl(lsci)

		// TODO:  Send any "go" commands to ari

		ari
	}
	
	override def disposeService(arz: ApproRaiz, availDeps : java.util.Map[String, java.lang.Object]): Unit = {
		warn0("ApproRaizLifecycle.disposeService called")
	}
	override def handleDependencyChange(arz: ApproRaiz, changeType: String, dependencyName: String, dep: AnyRef,
				availDeps: JavaMap[String, Object]): ApproRaiz = {
		throw new RuntimeException("Dep change not supported by ApproRaizLifecycle")
	}
	/*
	private def makeStaticUnaryDepSpec(depName : String, clzName : String) : ServiceDependency = {
		val card = ServiceDependency.Cardinality.MANDATORY_UNARY
		val upStrat = ServiceDependency.UpdateStrategy.STATIC
		val unusedExoticUsagePropsMap_forFutureUse_canBeNull = new JavaHashMap[String,String]()
		new ServiceDependency(depName, clzName, card, upStrat, unusedExoticUsagePropsMap_forFutureUse_canBeNull)

	}*/
}
import org.jflux.api.registry.Registry
import org.jflux.api.service.{DefaultRegistrationStrategy, RegistrationStrategy, ServiceManager}
import org.jflux.api.service.binding.ServiceBinding;
// import org.jflux.api.service.binding.ServiceBinding.BindingStrategy;
import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.BundleContext;

object ApproRaizLifeLauncher extends VarargsLogging with BindingMaker {
	// We keep track of this for debugging.  If a "boss bundle" comes home and finds someone in his bed, well, 
	// there may be trouble.
	private var		theParentCtx : Option[BundleContext] = None

	val jmodelClzName = classOf[JenaModel].getName
	val ehostClzName = classOf[EntryHost].getName
	val arClzName = classOf[ApproRaiz].getName

	val smClzName = classOf[ServiceManager[_]].getName
	
	// These are prop-keys+vals that ApproRaiz knows how to look for on its dependencies.
	// The keys, at least, should be defined in a more common API, possibly in the Circus layer.
	val		TAGKEY_MODEL_ROLE = "MODEL_ROLE"
	val		TAGVAL_MODEL_ROLE_RECIPES = "mRole_recipes"
	val		TAGVAL_MODEL_ROLE_SETTINGS = "mRole_settings"
	val		TAGVAL_MODEL_ROLE_STATUS = "mRole_status"
	
	val		TAGKEY_EHOST_ROLE = "EHOST_ROLE"
	val		TAGVAL_EHOST_ROLE_LEG_CONF = "ehRole_legConf"

	val		TAGKEY_RCPN_ROLE = "RCPNAME_ROLE"
	val		TAGVAL_RCPN_ROLE_MAIN = "rcpnRole_main"
	
	val		TAGKEY_ARAIZ_ROLE = "ARAIZ_ROLE"
	val		TAGKEY_ARAIZ_ROLE_MAIN = "aRaizRole_main"

	def launchServiceMgr(context : BundleContext) {
		if (theParentCtx.isEmpty) {
			val aRaizLife = new ApproRaizLifecycle()
			val bindingMap = new JavaHashMap[String,ServiceBinding]()

			appendModelRoleBinding(bindingMap, aRaizLife.myDepSpec_Recipes, TAGVAL_MODEL_ROLE_RECIPES)
			appendModelRoleBinding(bindingMap, aRaizLife.myDepSpec_Settings, TAGVAL_MODEL_ROLE_SETTINGS)
			appendModelRoleBinding(bindingMap, aRaizLife.myDepSpec_StatusOut, TAGVAL_MODEL_ROLE_STATUS)

			val regProps = new JavaHashMap[String, String]();
			regProps.put(TAGKEY_ARAIZ_ROLE, TAGKEY_ARAIZ_ROLE_MAIN);
			
			val svcRegStrat : RegistrationStrategy[ApproRaiz] = new DefaultRegistrationStrategy[ApproRaiz](Array[String](arClzName), regProps);
			val mngrRegStrat : RegistrationStrategy[ServiceManager[_]] = new DefaultRegistrationStrategy[ServiceManager[_]](Array[String](smClzName), null);
			val sm = new ServiceManager[ApproRaiz](aRaizLife, bindingMap, svcRegStrat, mngrRegStrat);
			val registry : Registry = new OSGiRegistry(context);
			sm.start(registry);
			theParentCtx = Some(context)
		} else {
			throw new RuntimeException("ApproRaiz Lifecycle was previously launched by context: " + theParentCtx)
		}
	}
	
	private def appendModelRoleBinding(bindingMap : JavaHashMap[String,ServiceBinding], 
									   dep : ServiceDependency, tagVal : String) : ServiceBinding = {
		appendEagerBinding(bindingMap, dep, TAGKEY_MODEL_ROLE, tagVal)
	}
	private def appendEHostBinding(bindingMap : JavaHashMap[String,ServiceBinding], 
									   dep : ServiceDependency, tagVal : String) : ServiceBinding = {
		appendEagerBinding(bindingMap, dep, TAGKEY_EHOST_ROLE, tagVal)
	}
	private def appendRcpNmBinding(bindingMap : JavaHashMap[String,ServiceBinding], 
									   dep : ServiceDependency, tagVal : String) : ServiceBinding = {
		appendEagerBinding(bindingMap, dep, TAGKEY_RCPN_ROLE, tagVal)
	}
}
import org.osgi.framework.ServiceRegistration

import scala.collection.JavaConversions._
object ApproRaizDepHelper extends VarargsLogging  {
	// These static methods can be used to register services directly, with the property-tags that our ApproRaizLifecycle is waiting for.
	def regModelSvc_Recipes(ctx : BundleContext, model : JenaModel) = regModelService(ctx, model, ApproRaizLifeLauncher.TAGVAL_MODEL_ROLE_RECIPES)
	def regModelSvc_Settings(ctx : BundleContext, model : JenaModel) = regModelService(ctx, model, ApproRaizLifeLauncher.TAGVAL_MODEL_ROLE_SETTINGS)
	def regModelSvc_StatusOut(ctx : BundleContext, model : JenaModel) = regModelService(ctx, model, ApproRaizLifeLauncher.TAGVAL_MODEL_ROLE_STATUS)

	def regRcpNmSvc_Main(ctx : BundleContext, rcpNms : ApproRaizRecipeNames) = regRecipeNameService(ctx, rcpNms, ApproRaizLifeLauncher.TAGVAL_RCPN_ROLE_MAIN)

	<!-- Issue with type parameters in OSGi classes, compiled under java 7, v 4.3.0 and 4.3.1
        http://blog.osgi.org/2012/10/43-companion-code-for-java-7.html
    -->

	private def regModelService(ctx : BundleContext, model : JenaModel, modelRole : String) : ServiceRegistration = { // [JenaModel] = {
		regServiceWithOneProp(ctx, classOf[JenaModel], model, ApproRaizLifeLauncher.TAGKEY_MODEL_ROLE, modelRole)
	}
	private def regEHostService(ctx : BundleContext, eHost : EntryHost, eHostRole : String) : ServiceRegistration = { // [EntryHost] =  {
		regServiceWithOneProp(ctx, classOf[EntryHost], eHost, ApproRaizLifeLauncher.TAGKEY_EHOST_ROLE, eHostRole)
	}
	private def regRecipeNameService(ctx : BundleContext, rcpNm : ApproRaizRecipeNames, rcpnRole : String) : ServiceRegistration = { // { [ApproRaizRecipeNames] =  {
		regServiceWithOneProp(ctx, classOf[ApproRaizRecipeNames], rcpNm, ApproRaizLifeLauncher.TAGKEY_RCPN_ROLE, rcpnRole)
	}	
	private def regServiceWithOneProp[ST](ctx : BundleContext, clz : Class[ST], svc : ST, 
							  propKey : String, propVal : String) : ServiceRegistration = { // [ST] = {
		val propMap = scala.collection.mutable.Map[String,String](propKey -> propVal)
		val propDict :  java.util.Dictionary[String,String] = propMap
		// ctx.registerService(clz, svc, propDict)
		ctx.registerService(clz.getName, svc, propDict)
	}
}