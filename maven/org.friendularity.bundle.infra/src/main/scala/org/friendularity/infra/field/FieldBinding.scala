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

package org.friendularity.infra.field

import org.appdapter.core.name.Ident

/**
  * Created by Stub22 on 7/14/2016.
  */
trait BoundToDataSrc
// Used to monitor detail on a known field of a known item.
trait FullyBoundToItemField extends BoundToDataSrc {
	def getAddress : ItemFieldSpec
}

trait BoundToFieldOfVariableItem extends BoundToDataSrc {
	def getFieldID : Ident
}

trait BindingUpdatePolicy {

}

