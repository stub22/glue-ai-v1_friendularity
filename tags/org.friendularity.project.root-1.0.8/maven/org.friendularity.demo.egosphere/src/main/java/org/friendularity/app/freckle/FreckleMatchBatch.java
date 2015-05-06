/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.freckle;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Stu Baurmann
 */
public class FreckleMatchBatch {
	public List<FreckleMatchCandidate>	myCandidates = new ArrayList<FreckleMatchCandidate>();
	public Long							mySleepMillisec = 0L;

	public static interface Supplier {
		public FreckleMatchBatch getFreckleMatchBatch();
	}
}
