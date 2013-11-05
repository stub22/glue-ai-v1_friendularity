package org.friendularity.jvision.filters;


import org.friendularity.jvision.gui.FileLocations;
/*
 * Detect facial profiles.
 * Only works going to the right (TBD - flip the image and run there)
 * Requires a slightly munged version of lbpcascade_profileface.xml
 * Delete the comment at the top of yours
 * 
 */
public class ProfileDetector extends CascadeDetector {
    
	@Override
	protected FileLocations.CascadeType cascadeType() {
		return FileLocations.CascadeType.PROFILE_CASCADE;
	}
}
