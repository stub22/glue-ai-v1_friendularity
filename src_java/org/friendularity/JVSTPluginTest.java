/*
 *  Copyright 2011 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity;

import jvst.wrapper.VSTPluginAdapter;
/**
 * @author Stu B. <www.texpedient.com>
 */
public class JVSTPluginTest extends VSTPluginAdapter {

	public JVSTPluginTest(long wrapperNum) {
	    super(wrapperNum);
	    log("Construktor JVSTPluginTest(" + wrapperNum + ") is loggink to you now!");
	}
	@Override public int getPlugCategory() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override public int canDo(String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override public boolean setBypass(boolean bln) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public boolean string2Parameter(int i, String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProgramNameIndexed(int i, int i1) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProductString() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getVendorString() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setParameter(int i, float f) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public float getParameter(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void processReplacing(float[][] floats, float[][] floats1, int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getProgram() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setProgram(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setProgramName(String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProgramName() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterName(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterDisplay(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterLabel(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getNumPrograms() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getNumParams() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

}
