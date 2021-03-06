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
package org.friendularity.jvision.broker;

/**
 * SwitchableImageStreamProducer adds the ability to 'bow out' to an ImageStreamProducer
 * Suppose we have SwitchableImageStreamProducer a and ImageStreamProducer b.
 * We want b to take over a's function. We would call a.switchTo(b) to move the consumers to b,
 * then remove a from the ImageStreamBroker and add b.
 * 
 * @author Annie
 */
interface SwitchableImageStreamProducer extends ImageStreamProducer {
	void switchTo(ImageStreamProducer p);
}
