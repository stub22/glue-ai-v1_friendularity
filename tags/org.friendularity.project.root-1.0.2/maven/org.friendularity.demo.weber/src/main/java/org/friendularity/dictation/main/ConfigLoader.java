/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author matt
 */
public class ConfigLoader {
	public static Map<String, Double> loadCostTable(String path) throws Throwable{
		Map<String, Double> table = new HashMap<String, Double>();
		File f = new File(path);
		FileReader fr = new FileReader(f);
		BufferedReader br = new BufferedReader(fr);
        if(f.length() > 0){
			while(br.ready()){
                String[] flds = br.readLine().split("" + (char)9);
				if(flds.length >= 2){
					String word = flds[0];
					Double valx = Double.parseDouble(flds[1]);
					table.put(word, valx);
				}
            }
            br.close();
        }
		return table;
	}
}
