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

package org.friendularity.anim.loader;

import java.util.List;
import org.osgi.framework.BundleContext;
import org.cogchar.platform.util.ClassLoaderUtils;

// The new two promiscuous imports allows running of both cogchar-1.0.6.2 and cogchar-1.0.7.0
import org.cogchar.blob.emit.*;
import org.appdapter.core.matdat.*;


/**
 * Picks up the animations referenced from the central repository and deposits
 * the AnimSpec for each into the JFlux registry.
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class AnimWiring {
    
    private final int ANIM_DEFAULT_NAMESPACE_SHEET_NUM = 9;
	private final int ANIM_DEFAULT_DIRECTORY_SHEET_NUM = 8;
    private final String ANIM_DEFAULT_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
    private final String ANIM_DEFAULT_WORKBOOK_PATH = "GluePuma_HRKR50_TestFull.xlsx";
    
    public void wire(BundleContext context, String sheetKey, int namespaceSheetNumber, int directorySheetNumber) {
//        List<ClassLoader> classloaders = ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
//        OnlineSheetRepoSpec repoSpec = OnlineSheetRepoSpec(sheetKey, namespaceSheetNumber, directorySheetNumber, classloaders);
//        
//        RepoConnector rc = new RepoConnector();
//        EnhancedRepoClient enhancedRepoSpec = rc.connectDemoRepoClient(repoSpec);
//        EnhancedRepoClient enhancedRepoSpec = null;
//        enhancedRepoSpec
    }    
}
