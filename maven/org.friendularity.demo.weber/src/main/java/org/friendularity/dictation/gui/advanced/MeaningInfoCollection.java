/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.gui.advanced;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JButton;

/**
 *
 * @author Eamq
 */
public class MeaningInfoCollection {

	private JButton myButton = new JButton();
	private List<String[]> myInfo = new ArrayList<String[]>();

	public MeaningInfoCollection(){}

	public MeaningInfoCollection(String vocab, String dest, String type){
		String[] info = new String[3];
		info[0] = vocab;
		info[1] = dest;
		info[2] = type;
		myInfo.add(info);
	}

	public List<String[]> getInfo(){
		return myInfo;
	}

	public JButton getButton(){
		return myButton;
	}

	public void setButton(JButton button){
		myButton = button;
	}

	public void addInfo(String vocab, String dest, String type){
		String[] info = new String[3];
		info[0] = vocab;
		info[1] = dest;
		info[2] = type;
		myInfo.add(info);
	}
}
