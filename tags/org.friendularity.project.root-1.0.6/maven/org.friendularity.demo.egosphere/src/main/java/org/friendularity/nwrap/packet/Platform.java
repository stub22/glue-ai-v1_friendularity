/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.packet;

import java.util.List;
import java.util.ArrayList;

/**
 *
 * @author Stu Baurmann
 */
public class Platform {
	public enum	 Kind {
		MSWIN_32,
		MSWIN_64,
		LINUX_32,
		LINUX_64
	}
	private	static	List<Kind>		theKindList;
	public static	List<Kind>		getPlatformKindEnumList() {
		if (theKindList == null) {
			theKindList = new ArrayList<Kind>();
			// Order here must be the same as order in the Enum decl, and 
			// as the order of integer constants on the C++ side.
			theKindList.add(Kind.MSWIN_32);
			theKindList.add(Kind.MSWIN_64);
			theKindList.add(Kind.LINUX_32);
			theKindList.add(Kind.LINUX_64);			
		}
		return theKindList;
	}		
		
}
