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
package org.friendularity.gui.bootstrap;

import java.util.List;
import org.appdapter.core.boot.ClassLoaderUtils;
import org.appdapter.core.repo.EnhancedRepoClient;
import org.appdapter.core.repo.OfflineXlsSheetRepoSpec;
import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.outer.behav.demo.RepoConnector;
import org.osgi.framework.BundleContext;

/**
 * This repo loader simplifies the interface for repo loading, allowing a single
 * call to load a repo regardless of its underlying type.
 *
 * @author Jason Randolph Eads <jeads362@gmail.com>
 */
public class ConsolidatedRepoLoader {

    /**
     * The list of handled repo types.
     */
    public enum RepoType {
        GOOGLE_SHEET_REPO,
        XLSX_FILE_REPO,
        TURTLE_FILE_REPO,
        SQL_DISK_REPO,
        CSS_FILE_REPO
    };

    /**
     * Load a repo into memory given the data needed to locate it.
     * 
     * @param repoType The kind of repo, as defined in local enum
     * @param repoPath The path to find the repo, or its name
     * @param namespacePath The path to find the namespace graph
     * @param directoryPath The path to find the directory graph
     * @param classloaders The JFlux context's classloaders
     * @return The connected repo.
     */
    public EnhancedRepoClient loadRepo(
            RepoType repoType,
            String repoPath,
            String namespacePath,
            String directoryPath,
            List<ClassLoader> classloaders) {
        
        // Pass data to appropriate loader method
        switch (repoType) {
            case XLSX_FILE_REPO:
                connectXLSXFileRepo(
                        repoPath, 
                        namespacePath, 
                        directoryPath, 
                        classloaders);
                break;

            case GOOGLE_SHEET_REPO:
            case TURTLE_FILE_REPO:
            case SQL_DISK_REPO:
                // TODO LOG - "Overloaded method call passed incorrect data for this repo type"
                return null;
            default:
                //TODO LOG - "Unhandled repo type error"
                return null;
        }
        return null;
    }

    /**
     * Load a repo into memory given the data needed to locate it.
     * 
     * @param repoType The kind of repo, as defined in local enum
     * @param repoPath The path to find the repo, or its name
     * @param namespaceSheetNumber The sheet number for the namespace graph
     * @param directorySheetNumber The sheet number for the directory graph
     * @param classloaders The JFlux context's classloaders
     * @return The connected repo.
     */
    public EnhancedRepoClient loadRepo(
            RepoType repoType,
            String repoPath,
            int namespaceSheetNumber,
            int directorySheetNumber,
            List<ClassLoader> classloaders) {
        
        // Pass data to appropriate loader method
        switch (repoType) {
            case GOOGLE_SHEET_REPO:
                connectGoogleSheetRepo(
                        repoPath,
                        namespaceSheetNumber,
                        directorySheetNumber,
                        classloaders);
                break;

            case TURTLE_FILE_REPO:
            case SQL_DISK_REPO:
            case XLSX_FILE_REPO:
                // TODO LOG - "Overloaded method call passed incorrect data for this repo type"
                return null;
            default:
                //TODO LOG - "Unhandled repo type error"
                return null;
        }
        return null;
    }

    /**
     * This connects to a GoogleSheet repo, given the raw data to do so.
     */
    private EnhancedRepoClient connectGoogleSheetRepo(
            String sheetKey,
            int namespaceSheetNumber,
            int directorySheetNumber,
            List<ClassLoader> classloaders) {
        
        // Build up the repoSpec, which contains the raw data for repo access
        OnlineSheetRepoSpec repoSpec = new OnlineSheetRepoSpec(
                sheetKey,
                namespaceSheetNumber,
                directorySheetNumber,
                classloaders);
        
        // Connect to the repo.
        RepoConnector rc = new RepoConnector();
        EnhancedRepoClient enhancedRepoSpec =
                rc.connectDemoRepoClient(repoSpec);
        
        return enhancedRepoSpec;
    }

    /**
     * This connects to a Turtle File repo, given the raw data to do so.
     */
//    private EnhancedRepoClient connectTurtleFileRepo(

    
    /**
     * This connects to a SQL Disk repo, given the raw data to do so.
     */
//    private EnhancedRepoClient connectSQLDiskRepo(
    
    
    /**
     * This connects to a CSS File repo, given the raw data to do so.
     */
//    private EnhancedRepoClient connectCSSFileRepo(
    
     /**
     * This connects to a XLSX File repo, given the raw data to do so.
     */
    private EnhancedRepoClient connectXLSXFileRepo(
            String repoPath,
            String namespacePath,
            String directoryPath,
            List<ClassLoader> classloaders) {
        
        // Build up the repoSpec, which contains the raw data for repo access
        OfflineXlsSheetRepoSpec repoSpec = new OfflineXlsSheetRepoSpec(
                repoPath,
                namespacePath,
                directoryPath,
                classloaders);
        
        // Connect to the repo.
        RepoConnector rc = new RepoConnector();
        EnhancedRepoClient enhancedRepoSpec = rc.connectDemoRepoClient(repoSpec);
        
        return enhancedRepoSpec;
    }
}
