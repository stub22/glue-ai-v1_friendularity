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
import org.appdapter.bind.rdf.jena.assembly.DynamicCachingComponentAssembler
import org.appdapter.core.item.{Item}
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.assembler.{Assembler, Mode}

/**
 * The data object for a property.
 *
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 */
class PropertySpec extends KnownComponentImpl {
  
  // The stored data definitions
  var myName: String
  var myValue: String
  
  // Setters for data
  def setName(name: String) {myName = name}
  def setValue(value: String) {myValue = value}
  
  // Getters for data
  def getName(): String = {myName}
  def getValue(): String = {myValue}
}

/**
 * The builder that is called to make a Spec object from the raw RDF data
 * representing a Property.
 *
 * @author Jason Randolph Eads <eadsjr@hansonrobokind.com>
 */
class PropertySpecBuilder( builderConfigurationResource: Resource ) extends DynamicCachingComponentAssembler[PropertySpec](builderConfigurationResource) {
  override protected def initExtendedFieldsAndLinks(pSpec: PropertySpec, configItem : Item, assmblr : Assembler , mode: Mode ) {
    val itemAssemblyReader = getReader();
    
    // Defines the relationship "#Property Name" key (aka the Predicate), that is followed from an individual to collect the data
    val propertyKeyId : String = "propKey";
    val propertyValueId : String = "propValue";

    // reads in the data fields and stores them in the Spec
    pSpec.setName(itemAssemblyReader.readConfigValString(configItem.getIdent(), propertyKeyId, configItem, ""))
    pSpec.setValue(itemAssemblyReader.readConfigValString(configItem.getIdent(), propertyValueId, configItem, ""))
  }
}
