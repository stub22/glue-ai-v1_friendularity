package org.friendularity.vw.impl.manip

/**
  * Code moved to new file on 1/19/2017.
  */
trait JmeBoneAnimHlp {
	// Rendering happens on callbacks to the tracks that look like:
	// void setTime(float time, float weight, AnimControl control, AnimChannel channel, TempVars vars);
	// BoneTrack constructor is:
	// BoneTrack(int targetBoneIndex, float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
	// While SpatialTrack constructor is:
	// SpatialTrack(float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
	// It does:
	/*
		tempQ.nlerp(tempQ2, blend);
		tempV.interpolateLocal(tempV2, blend);
		tempS.interpolateLocal(tempS2, blend);
	 */
}
