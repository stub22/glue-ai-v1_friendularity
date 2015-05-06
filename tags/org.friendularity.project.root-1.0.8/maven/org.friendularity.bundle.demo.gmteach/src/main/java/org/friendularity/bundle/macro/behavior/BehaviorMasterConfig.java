/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.behavior;

import java.util.List;

import org.appdapter.core.matdat.OfflineXlsSheetRepoSpec;
import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.repo.URLRepoSpec;
import org.cogchar.platform.util.ClassLoaderUtils;
import org.osgi.framework.BundleContext;

/**
 *
 * @author Yishuai Li & Jason Eads
 */
public class BehaviorMasterConfig {

    private final static String BMC_SOURCE = "com.hrkind.demo.behavior.master.source";
        
    private final static String BMC_DEFAULT_SHEET_KEY = "0AlpQRNQ-L8QUdFh5YWswSzdYZFJMb1N6aEhJVWwtR3c";
    private final static int BMC_DEFAULT_NAMESPACE_SHEET_NUM = 4;
    private final static int BMC_DEFAULT_DIRECTORY_SHEET_NUM = 3;
    private final static String BMC_DEFAULT_WORKBOOK_PATH = "GluePuma_BehavMasterDemo.xlsx";
    private final static String BMC_DEFAULT_WORKBOOK_NAMESPACE_SHEET = "Nspc.csv";
    private final static String BMC_DEFAULT_WORKBOOK_DIRECTORY_SHEET = "Dir.csv";
    
    private boolean isEnvRead = false;
    private String SheetKey;
    private int NamespaceSheetNum;
    private int DirectorySheetNum;
    private String WorkbookPath;
    private String WorkbookNamespaceSheet;
    private String WorkbookDirectorySheet;
    
    public final static String BMC_RECORD_DELIMETER = ",";
    public final static String BMC_FIELD_DELIMETER = ";";
    
    public final static String BMC_SHEET_KEY_FIELD_NAME = "sheetKey";
    public final static String BMC_NAMESPACE_SHEET_NUMBER_PATH_FIELD_NAME = "namespaceSheetNumber";
    public final static String BMC_DIRECTORY_SHEET_NUMBER_PATH_FIELD_NAME = "directorySheetNumber";
    public final static String BMC_WORKBOOK_PATH_FIELD_NAME = "workBookPath";
    public final static String BMC_WORKBOOK_NAMESPACE_SHEET_FIELD_NAME = "Nspc.csv";
    public final static String BMC_WORKBOOK_DIRECTORY_SHEET_FIELD_NAME = "Dir.csv";
    
    
    /**
     * Loads the repo source data from the configuration.
     */
    synchronized public void loadDataFromConfig() {
    	if (isEnvRead) return;
    	isEnvRead = true;
        // Set the defaults, which will hold if the config read fails for any reason
        SheetKey = BMC_DEFAULT_SHEET_KEY;
        NamespaceSheetNum = BMC_DEFAULT_NAMESPACE_SHEET_NUM;
        DirectorySheetNum = BMC_DEFAULT_DIRECTORY_SHEET_NUM;
        WorkbookPath = BMC_DEFAULT_WORKBOOK_PATH;
        WorkbookNamespaceSheet = BMC_DEFAULT_WORKBOOK_NAMESPACE_SHEET;
        WorkbookDirectorySheet = BMC_DEFAULT_WORKBOOK_DIRECTORY_SHEET;

        // Get the target sheet or workbook from the configuration
        String envVar = System.getProperty(BMC_SOURCE, System.getenv(BMC_SOURCE));
        if (envVar != null && !envVar.isEmpty()) {
            String[] records = envVar.split(BMC_RECORD_DELIMETER);
            for (String record : records) {
                String[] fields = record.split(BMC_FIELD_DELIMETER);
                if(fields.length < 2){
                    continue;
                }
                fields[0] = fields[0].trim();
                fields[1] = fields[1].trim();

                // The first field of the record is the Key
                if (fields[0].equals(BMC_SHEET_KEY_FIELD_NAME)) {
                    SheetKey = fields[1];
                } else if (fields[0].equals(BMC_NAMESPACE_SHEET_NUMBER_PATH_FIELD_NAME)) {
                    NamespaceSheetNum = Integer.parseInt(fields[1]);
                } else if (fields[0].equals(BMC_DIRECTORY_SHEET_NUMBER_PATH_FIELD_NAME)) {
                    DirectorySheetNum = Integer.parseInt(fields[1]);
                } else if (fields[0].equals(BMC_WORKBOOK_PATH_FIELD_NAME)) {
                    WorkbookPath = fields[1];
                } else if (fields[0].equals(BMC_WORKBOOK_NAMESPACE_SHEET_FIELD_NAME)) {
                    WorkbookNamespaceSheet = fields[1];
                } else if (fields[0].equals(BMC_WORKBOOK_DIRECTORY_SHEET_FIELD_NAME)) {
                    WorkbookDirectorySheet = fields[1];
                }
            }
        }
    }
    
    /**
     * Prepares object to connect to local spreadsheet repo as defined in the configuration.
     * 
     * @param context JFlux bundle context
     * @return Unconnected repo object
     */
    public OfflineXlsSheetRepoSpec makeBMC_OfflineXlsSheetRepoSpecOLD(BundleContext context) {
        loadDataFromConfig();
        List<ClassLoader> classloaders = ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        return new OfflineXlsSheetRepoSpec(WorkbookPath, WorkbookNamespaceSheet, WorkbookDirectorySheet, classloaders);
    }

    /**
     * Prepares object to connect to local spreadsheet repo as defined in the configuration.
     * 
     * @param context JFlux bundle context
     * @return Unconnected repo object
     */
    public URLRepoSpec makeBMC_OfflineXlsSheetRepoSpec(BundleContext context) {
        loadDataFromConfig();
        List<ClassLoader> classloaders = ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        return new URLRepoSpec(WorkbookPath, classloaders);
    }

    
    /**
     * Prepares object to connect to online repo as defined in the configuration.
     * 
     * @param context JFlux bundle context
     * @return Unconnected repo object
     */
    public OnlineSheetRepoSpec makeBMC_OnlineSheetRepoSpec(BundleContext context) {
        loadDataFromConfig();
        List<ClassLoader> classloaders = ClassLoaderUtils.getFileResourceClassLoaders(context, ClassLoaderUtils.ALL_RESOURCE_CLASSLOADER_TYPES);
        return new OnlineSheetRepoSpec(SheetKey, NamespaceSheetNum, DirectorySheetNum, classloaders);
    }

	public void setWorkbookPath(String envVarURLValue) {
		WorkbookPath = envVarURLValue;		
	}
}
