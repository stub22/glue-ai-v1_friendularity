/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.webber.utils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;

/**
 *
 * @author matt
 */
public class FileHelper {
	public static void saveTextFile(String data, String path) throws Throwable {
		FileWriter fstream = new FileWriter(path);
		BufferedWriter out = new BufferedWriter(fstream);
		out.write(data);
		out.close();
	}

	public static String getTextFile(String path) throws Throwable {
		File file = new File(path);
		FileInputStream fis = new FileInputStream(file);
		BufferedReader br = new BufferedReader(new InputStreamReader(fis));
		String contents = "";
		while (br.ready()) {
			contents += br.readLine() + "\n";
		}
		fis.close();
		return contents;
	}
}
