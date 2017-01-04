package org.friendularity.netcli.goodtst

import org.cogchar.name.goody.GoodyNames

/**
  * Created by Owner on 1/4/2017.
  */
trait GoodyTypeAliases {
	// Cogchar~2013 had 9 real kinds of goody and 2 pseudo-goody-kinds, rarely used to refer to avatars and cameras.
	// 6 Goody kinds are deep, 3 are flat.  4 of the deep goody kinds carry additional application state, as does
	// the flat-scoreboard kind.  The flat-crosshair kind depends on measured actual openGL canvas size.
	// The deep-tictac kinds have an optional parent-child relationship.

	// BEGIN Our 6 deep (3D) kinds, starting with the 4 more stateful kinds
	val GT_BIT_BOX = GoodyNames.TYPE_BIT_BOX

	val GT_BIT_CUBE = GoodyNames.TYPE_BIT_CUBE  // Requires Robosteps textures

	val GT_TICTAC_GRID = GoodyNames.TYPE_TICTAC_GRID  // Does not yet support MOVE

	val GT_TICTAC_MARK = GoodyNames.TYPE_TICTAC_MARK

	// Now the 2 less-stateful deep goodies
	val GT_BOX = GoodyNames.TYPE_BOX
	val GT_FLOOR = GoodyNames.TYPE_FLOOR     // Ignores rotation on CREATE, but respects on MOVE

	// BEGIN our 3 flat (2D) kinds
	// 2D, uses absolute pixel pos
	val GT_TEXT  = GoodyNames.TYPE_TEXT
	// Scoreboard has some substate of rows, etc
	val GT_SCOREBOARD = GoodyNames.TYPE_SCOREBOARD

	// 2D, requires fractional screen pos
	val GT_CROSSHAIR = GoodyNames.TYPE_CROSSHAIR


	// BEGIN - Not really goody types - vestigal to old Cogchar-2013, to be removed in early 2017.
	val GT_AVATAR = GoodyNames.TYPE_AVATAR
	val GT_CAMERA = GoodyNames.TYPE_CAMERA
}
