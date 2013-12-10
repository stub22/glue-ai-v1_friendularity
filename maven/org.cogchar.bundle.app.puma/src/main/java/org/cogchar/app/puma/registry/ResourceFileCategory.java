/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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
package org.cogchar.app.puma.registry;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public enum ResourceFileCategory {
	RESFILE_REPO_RDF, // Not used yet - 2012-10-23
	RESFILE_ANIM_XML, // Not used yet - 2012-10-23
	RESFILE_OPENGL_JME3_OGRE, // Not used yet - 2012-10-23
	RESFILE_WEB_LIFT, // Not used yet - 2012-10-23
	RESFILE_WEB_SPARQL, // Not used yet - 2012-10-23
	RESFILE_RK_CONF // Used for Robokind JointGroup file open via getResourceAsStream
}
