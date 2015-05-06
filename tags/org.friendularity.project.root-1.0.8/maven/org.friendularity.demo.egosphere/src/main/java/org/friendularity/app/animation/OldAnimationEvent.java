/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

/**
 * @author Stu Baurmann
 */
public class OldAnimationEvent {
	public enum Type {
		COMPLETE
	};
	public	Type					myType;
	public OldExecutingAnimation		myAnimation;
}
