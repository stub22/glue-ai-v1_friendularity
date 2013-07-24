 /*
 * Copyright 2013 The Friendularity Project (www.friendularity.org).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.friendularity.spec.connection

import org.appdapter.core.component.KnownComponentImpl
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler
import org.appdapter.core.item.{Item}
import org.appdapter.core.name.{Ident}
import com.hp.hpl.jena.assembler.{Assembler, Mode}
import scala.collection.mutable

/**
 * The data object for the dependency binding properties of a service.
 *
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 */
class ServiceBindingSpec extends KnownComponentImpl {

  // The stored data definitions
  var className: String
  val properties = Map.empty[String,String]
  
  // Setters for data
  def setClassName(classname: String) { className = classname }
  
  // Accumulators for data
  def addProperty(key:String, value:String) { properties + (key->value) }
  
  // Getters for data
  def getClassName(): String = { className }
  def getProperties(): Map[String,String] = { properties }
}

/**
 * The builder that is called to make a Spec object from the raw RDF data
 * representing a ServiceBinding.
 * 
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 */
 class ServiceBindingSpecBuilder extends CachingComponentAssembler[ServiceBindingSpec] {

  override protected def decideComponentClass(ident: Ident, item: Item): Class[ServiceBindingSpec] = {
    return classOf[ServiceBindingSpec]
  }
  
  override protected def initExtendedFieldsAndLinks(sbSpec: ServiceBindingSpec, configItem : Item, assmblr : Assembler , mode: Mode ) {
    val itemAssemblyReader = getReader()
    
    // Defines the relationship "#Property Name" key (aka the Predicate), that is followed from an individual to collect the data
    val serviceClassNameId = "serviceJavaFQCN"
    val servicePropertiesId = "hasProperty";
    
    // read in the data field and store it in the Spec
    sbSpec.setClassName(itemAssemblyReader.readConfigValString(configItem.getIdent(), serviceClassNameId, configItem, ""))
    
    // read in and build the linked properties, and storing each in the Spec
    val linkedProperties : java.util.List[Object] = itemAssemblyReader.findOrMakeLinkedObjects(configItem, servicePropertiesId, assmblr, mode, null)
    for( o <- linkedProperties) {
      o match {
        case propertySpec: PropertySpec => sbSpec.addProperty(propertySpec.getName, propertySpec.getValue)
        case _ => getLogger.warn("Unexpected object found at {} = {}", Array[Object]( servicePropertiesId, o));
      }
    }
   }
 }
 