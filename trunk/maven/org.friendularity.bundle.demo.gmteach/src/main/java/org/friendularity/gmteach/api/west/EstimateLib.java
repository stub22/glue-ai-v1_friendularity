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
package org.friendularity.gmteach.api.west;

import java.util.HashSet;
import java.util.Set;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;

import static org.friendularity.gmteach.api.west.WorldEstimate.ESTIM_NS;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class EstimateLib {

	public static Set<PersonEstimate> makeFunPersonEstims() {
		Set<PersonEstimate> sampleSet = new HashSet<PersonEstimate>();
		for (int i = 0; i < 10; i++) {
			Ident personID = new FreeIdent(ESTIM_NS + "person_estim_0" + i);
			PersonEstimate pest = new PersonEstimate(personID, i);
			double px = -10.0 - i * 2.0, py = 10.0 + i * 2.0, pz = 4.0;
			String baseVecExpr = "{" + px + ", " + py + ", " + pz + "}";
			pest.setPosMathExpr("$multA * Sin[$phaseAng]*Sqrt[1.0 * $personIdx]*" + baseVecExpr);
			pest.setColorMathExpr("{Sin[$phaseAng],Sin[3.0*$phaseAng], Sin[$phaseAng/2], $phaseFrac}");
			sampleSet.add(pest);
		}
		return sampleSet;
	}

	public static Set<StuffEstimate> makeFunStuffEstims(int jMax, int kMax) {
		Set<StuffEstimate> sampleSet = new HashSet<StuffEstimate>();
		for (int j = 1; j <= jMax; j++) {
			// TODO - format the index number
			Ident stuffID = new FreeIdent(ESTIM_NS + "stuff_estim_0" + j);
			StuffEstimate sest = new StuffEstimate(stuffID, j);
						// WorldEstimate.StuffEstimate.Kind.REGULAR);
			double px = 15.0 + j * 3.0, py = 10.0 + j * 3.0, pz = -12.0;
			// Making use of single-threaded access to MathSpace:
			// We assume that someone will post our stuffIdx before each eval of the following.
			String baseVecExpr = "{" + px + ", " + py + ", " + pz + "}";
			sest.setPosMathExpr("$multA*Sin[$phaseAng]*" + baseVecExpr);
			sest.setColorMathExpr("{Sin[$stuffIdx * $phaseAng],Sin[3.0 * $phaseAng], Sin[$phaseAng/(2 * $stuffIdx)], $phaseFrac * $stuffIdx}");
			sampleSet.add(sest);
		}

		for (int k = 1; k <= kMax; k++) {
			int stuffIdx = k + jMax;
			Ident stuffID = new FreeIdent(ESTIM_NS + "stuff_estim_0" + stuffIdx);
			StuffEstimate sest = new StuffEstimate(stuffID, stuffIdx); 
					// , WorldEstimate.StuffEstimate.Kind.MONSTER);
			double px = 15.0 + k * 3.0, py = 10.0 + k * 3.0, pz = -25.0;
			double kFrac = k / (double) kMax;
			double red = 0.4, green = 0.2, blue = 0.8, alpha = kFrac;
			sest.setColorMathExpr("{" + red + ", " + green + ", " + blue + ", " + alpha + "}");
			double phaseOff = kFrac * 2 * Math.PI;
			sest.setPosMathExpr("$phaseTot:=$phaseAng + " + phaseOff + "; {20.0*Cos[$phaseTot], $phaseTot * 5.0, 15.0*Sin[$phaseTot]}");
			sampleSet.add(sest);
		}
		return sampleSet;
	}
}
