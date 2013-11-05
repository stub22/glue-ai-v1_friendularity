/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.model.joint;

import java.awt.Rectangle;
import org.cogchar.api.animoid.config.bonus.MuscleJoint;

/**
 * @author Stu Baurmann
 */
public class OldSightDirectionModel {
	// In C++ impl, Observables can tell us which of these applies to them..
	public enum OffScreenDirection {
		OnScreen,
		Left,
		Right,
		Above,
		Below,
		AboveLeft,
		AboveRight,
		BelowLeft,
		BelowRight,
		Unknown
	};

	Object		m_currentFocus = null;
	Object		m_previousFocus = null;
	
	int m_eyesVerticalServo = MuscleJoint.BothEyes_Up.getJointID();
	int m_bothEyesTurnServo = MuscleJoint.BothEyes_TurnRight.getJointID();
	//int m_rightEyeTurnServo = MuscleJoint.RightEye_TurnRight;
	//int m_leftEyeTurnServo = MuscleJoint.LeftEye_TurnRight;
	int m_headTurnServo = MuscleJoint.Head_TurnRight.getJointID();
	int m_headUpperNodServo = MuscleJoint.UpperNod_Forward.getJointID();
	//int m_headLowerNodServo = MuscleJoint.LowerNod_Forward;

	//  : m_rightEyePointToMoveMultiplier(-0.2f), m_eyesVerticalPointToMoveMultiplier(-0.07f),
//    m_upperNodPointToMoveMultiplier(0.05f), m_lowerNodPointToMoveMultiplier(0.0f), 
//    m_headTurnPointToMoveMultiplier(0.08f),

	
	// Empirically - using HP webcam
	/*
	 * object 1 unit wide fills the view horizontally at a distance of 1.7 units
	 * object 1 unit high fills the view vertically at a distance of 2 units.
	 * 
	 * horiz fov angle = 2 * arctan (0.5 /  1.7) ~= 32 degrees = .57 radians
	 * vert  fov angle = 2 * arctan (0.5 / 2.0) ~= 28 degrees  = .49 radians = 
	 */ 
	
	int m_fovWidth = 320;  // width of the field of view - in pixels
	int m_fovHeight = 240; // height of the field of view - in pixels

	// these are the multiplier that converts the number of pixels in the fov to a servo percentage move
	float m_bothEyesPointToMoveMultiplier = -2.0f/(320*8);
	float m_eyesVerticalPointToMoveMultiplier = -2.0f/(240*8);
	//m_rightEyePointToMoveMultiplier(-2.0f/(320*8)), 
    float m_upperNodPointToMoveMultiplier = -2.0f/(240*11);
	float m_lowerNodPointToMoveMultiplier = 0.0f;
	float m_headTurnPointToMoveMultiplier = -2.0f/(320*11);
	
	// range of distances in which to move the eyes only when looking at something
	int	 m_lookEyeOnlyMoveRange = 20;
	
	// range of distances in which to move the eyes only when glancing at something
	int  m_glanceEyeOnlyMoveRange = 30;
	
	// amount of give a face is allowed to have off center
	float m_centerSlack = 0.05f;
	
    float m_vergenceBaseDiff = 0.32f;
	float m_vergenceDistMult = 0.006f;

	float m_maxDelta = 0.5f;
	float m_maxNodDelta = 0.3f;
    
	boolean m_neckMoving = false;
	boolean m_eyesMoving = true;

	float m_eyesComboScalingFactor = 1.0f; 
    float m_neckComboScalingFactor = 2.0f;
	
	public double getCurrentJointPosition(int jointID) {
		double result = 0.0;
		//  result = head.queryPosition(jointID)
		return result;
	}

/*
 *
				bool isCentered(CvRect pos); // Is this rect centered
	

void AttentionRule::
DoCalculateMotion(os_time& time, WeightedBlendingMotionFrame& frame, 
		com::hansonrobotics::controlsystem::HeadControlSystem& head)
{
  os_sem_lock< os_non_recursive_mutex > guard( m_attentionChangeLock );

  // something is under observation
  //if ( m_currentFocus.get() != 0 )
  if ( m_currentFocus != 0 )
  {
    CvRect pos = m_currentFocus->getPosition();
    if ( pos.x != -1 )
    {
bool copiousDebug = FALSE;
	if (copiousDebug) {
			std::cout << "Att rule: (" << pos.x << ", " << pos.y << ") "
      			<< "size: (" << pos.width << ", " << pos.height << ")" << std::endl;
		}
      // it's onscreen
      int faceXFocus = static_cast<int>(floor(pos.x + .4 * pos.width));
      int faceYFocus = static_cast<int>(floor(pos.y + .33 * pos.height));
      int horzOffset = static_cast<int>(floor(m_fovWidth/2.0 - faceXFocus));
      int vertOffset = static_cast<int>(floor(m_fovHeight/2.0 - faceYFocus));

      double zDist = -2.3 * pos.width + 192;
      double inPerPixel = 7.0 / pos.width;
      double horzAngle = tan( horzOffset * inPerPixel / zDist );
      double vertAngle = tan( vertOffset * inPerPixel / zDist );

	if (copiousDebug) {
      std::cout << "faceXFocus: " << faceXFocus << " faceYFocus: " << faceYFocus
      	<< " horzOffset: " << horzOffset << " vertOffset: " << vertOffset << std::endl;
      std::cout << "zDist: " << zDist << " inPerPixel: " << inPerPixel
      		<< " horzAngle: " << horzAngle << " vertAngle: " << vertAngle << std::endl;
	}



      if ( abs(horzAngle) < m_centerSlack && abs(vertAngle) < m_centerSlack )
        MaintainPosition(time, frame, head);
      else
      {
        //if ( abs(head.QueryPosition(m_eyesVerticalServo)) > .2
    	// || abs(head.QueryPosition(m_leftEyeTurnServo)) > .2 )
        //  m_neckMoving = true;
        //if ( abs(head.QueryPosition(m_leftEyeTurnServo)) < .05
        //     && abs(head.QueryPosition(m_leftEyeTurnServo)) < .05 )     // ASK JOSH ABOUT THIS!!
        // m_neckMoving = false;
        if ( abs(head.QueryPosition(m_eyesVerticalServo)) > .2
    	 || abs(head.QueryPosition(m_bothEyesTurnServo)) > .2 )
          m_neckMoving = true;
        if ( abs(head.QueryPosition(m_eyesVerticalServo)) < .05
             && abs(head.QueryPosition(m_bothEyesTurnServo)) < .05 )
         m_neckMoving = false;
        
        //MoveHorz(horzAngle, time, frame, head);
        //MoveVert(vertAngle, time, frame, head);
        MoveHorz(horzOffset, time, frame, head);
        MoveVert(vertOffset, time, frame, head);
        calculateVergence(zDist, horzAngle, time, frame, head);
      }
    }
    else
    {
      // handle offscreen object
      // XXX later
      MaintainPosition(time, frame, head);
    }
  }
}
*/
// CvRect
public boolean isCentered(Rectangle pos) {
	int leftOffset = pos.x;
	int topOffset = pos.y;
	int rightOffset = m_fovWidth - ( pos.x + pos.width );
	int bottomOffset = m_fovHeight - ( pos.y + pos.height );
  
	if ((Math.abs(leftOffset - rightOffset) < m_centerSlack)
				&&  (Math.abs(topOffset - bottomOffset) < m_centerSlack)) {
		return true;
	} else {
		return false;
	}
}

/*
 os_time& time, WeightedBlendingMotionFrame& frame, 
	  com::hansonrobotics::controlsystem::HeadControlSystem& head)
 */
void moveHorz(double angle, Object time, Object frame, Object head) {
/*
#ifdef USE_JOYSTICK
  if ( m_joyStick.get() == 0 )
    return;

  JoystickDuctDeviceState js;

  // Get the input's device state
  m_joyStick->copyState( js );

  if ( js.rgbButtons[3] & 0x80 && js.lZ == 1000 ) {
    m_rightEyePointToMoveMultiplier = js.lY / 2000.0;
    std::cout << "rightEyeMult: " << m_rightEyePointToMoveMultiplier << std::endl;
  }

  if ( js.rgbButtons[3] & 0x80 && js.lZ == -1000 ) {
    m_headTurnPointToMoveMultiplier = js.lY / 2000.0;
    std::cout << "headTurnMult: " << m_headTurnPointToMoveMultiplier << std::endl;
  }
#endif
  */
	double origBothEyesPosition =  getCurrentJointPosition(m_bothEyesTurnServo);
  
  //std::cout << "Position: " << OrigBothEyesPosition
  //	    << " Delta: " << BothEyesDelta;
	if (m_eyesMoving) {

		double bothEyesDelta = angle * m_bothEyesPointToMoveMultiplier;
		bothEyesDelta = checkChangeBounds(bothEyesDelta, angle);
		if ( m_neckMoving ) {
			bothEyesDelta /= m_eyesComboScalingFactor;
		}
	 

    double bothEyesPosition = origBothEyesPosition + bothEyesDelta;
    bothEyesPosition = checkBounds(bothEyesPosition);

    //std::cout << "Move right to: " << BothEyesPosition << std::endl;
    // frame.AddMjMovement( BlendRuleWeights::ATTENTION, 
	//	std::make_pair(m_bothEyesTurnServo, BothEyesPosition) );
  }

  if ( m_neckMoving )
  {
    double origHeadTurnPosition = getCurrentJointPosition(m_headTurnServo); // head.QueryPosition(m_headTurnServo);
    double headTurnPositionDelta = angle * m_headTurnPointToMoveMultiplier;
	if ( Math.abs(origBothEyesPosition) > .7 ) {
		headTurnPositionDelta *= 2;
	}
    headTurnPositionDelta = checkChangeBounds(headTurnPositionDelta, angle);
    if ( m_eyesMoving ) {
		headTurnPositionDelta /= m_neckComboScalingFactor;
	}
	
  
    //std::cout << "Position: " << OrigEyesVerticalPosition
    //    << " Delta: " << EyesVerticalPositionDelta;
  
    double headTurnPosition = origHeadTurnPosition + headTurnPositionDelta;
    headTurnPosition = checkBounds(headTurnPosition);
    //std::cout << "Move down to: " << EyesVerticalPosition << std::endl;
    // frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headTurnServo, HeadTurnPosition) );
  }
  else
  {
    double origPosition = getCurrentJointPosition(m_headTurnServo);
    // frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headTurnServo, origPosition) );
  }
 
}
/*
void AttentionRule::
MoveVert(double angle, os_time& time, WeightedBlendingMotionFrame& frame, 
	 com::hansonrobotics::controlsystem::HeadControlSystem& head)
{
#ifdef USE_JOYSTICK
  if ( m_joyStick.get() == 0 )
    return;

  JoystickDuctDeviceState js;

  // Get the input's device state
  m_joyStick->copyState( js );

  if ( js.rgbButtons[0] & 0x80 && js.lZ == 1000 ) {
    m_eyesVerticalPointToMoveMultiplier = js.lY / 5000.0;
    std::cout << "eyeVertMult: " << m_eyesVerticalPointToMoveMultiplier << std::endl;
  }
  if ( js.rgbButtons[1] & 0x80 && js.lZ == -1000 ) {
    m_upperNodPointToMoveMultiplier = js.lY / 5000.0;
    std::cout << "upperNodMult: " << m_upperNodPointToMoveMultiplier << std::endl;
  }
  if ( js.rgbButtons[2] & 0x80 && js.lZ == -1000 ) {
    m_lowerNodPointToMoveMultiplier = js.lY / 5000.0;
    std::cout << "lowerNodMult: " << m_lowerNodPointToMoveMultiplier << std::endl;
  }
#endif

  double OrigEyesVerticalPosition = head.QueryPosition(m_eyesVerticalServo);
  
  //std::cout << "Position: " << OrigEyesVerticalPosition
  //    << " Delta: " << EyesVerticalPositionDelta;
    
  if ( m_eyesMoving )
  {
    double EyesVerticalPositionDelta = angle * m_eyesVerticalPointToMoveMultiplier;
    CheckChangeBounds(EyesVerticalPositionDelta, angle);
    if ( m_neckMoving )
      EyesVerticalPositionDelta /= m_eyesComboScalingFactor;

    double EyesVerticalPosition = OrigEyesVerticalPosition + EyesVerticalPositionDelta;
    CheckBounds(EyesVerticalPosition);
    //std::cout << "Move down to: " << EyesVerticalPosition << std::endl;
    frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_eyesVerticalServo, EyesVerticalPosition) );
  }

  if ( m_neckMoving )
  {
    double OrigUpperNodPosition = head.QueryPosition(m_headUpperNodServo);
    double UpperNodPositionDelta = angle * m_upperNodPointToMoveMultiplier;
    if ( abs(OrigEyesVerticalPosition) > .7 )
      UpperNodPositionDelta *= 2;
    CheckChangeBounds(UpperNodPositionDelta, angle);
    if ( m_eyesMoving )
      UpperNodPositionDelta /= m_neckComboScalingFactor;
  
    //std::cout << "Position: " << OrigEyesVerticalPosition
    //    << " Delta: " << EyesVerticalPositionDelta;
  
    double UpperNodPosition = OrigUpperNodPosition + UpperNodPositionDelta;
    CheckBounds(UpperNodPosition);
    //std::cout << "Move down to: " << EyesVerticalPosition << std::endl;
    frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headUpperNodServo, UpperNodPosition) );

    //ZenoPort double OrigLowerNodPosition = head.QueryPosition(m_headLowerNodServo);
    double LowerNodPositionDelta = angle * m_lowerNodPointToMoveMultiplier;
    if ( abs(OrigEyesVerticalPosition) > .9 )
      UpperNodPositionDelta *= 2;
    CheckChangeBounds(LowerNodPositionDelta, angle);
  
    //std::cout << "Position: " << OrigEyesVerticalPosition
    //    << " Delta: " << EyesVerticalPositionDelta;
  
    //ZenoPort double LowerNodPosition = OrigLowerNodPosition + LowerNodPositionDelta;
    //ZenoPort CheckBounds(LowerNodPosition);
    //std::cout << "Move down to: " << EyesVerticalPosition << std::endl;
    //frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headLowerNodServo, LowerNodPosition) );
  }
  else
  {
    double origPosition = head.QueryPosition(m_headUpperNodServo);
    frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headUpperNodServo, origPosition) );
  }
}

void AttentionRule::
MaintainPosition(os_time& time, WeightedBlendingMotionFrame& frame, 
       com::hansonrobotics::controlsystem::HeadControlSystem& head)
{
  double origPosition = head.QueryPosition(m_eyesVerticalServo);
  frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_eyesVerticalServo, origPosition) );
  //origPosition = head.QueryPosition(m_leftEyeTurnServo);
  //frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_leftEyeTurnServo, origPosition) );
  //origPosition = head.QueryPosition(m_rightEyeTurnServo);
  //frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_rightEyeTurnServo, origPosition) );
  origPosition = head.QueryPosition(m_bothEyesTurnServo);
  frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_bothEyesTurnServo, origPosition) );

  origPosition = head.QueryPosition(m_headTurnServo);
  frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headTurnServo, origPosition) );
  origPosition = head.QueryPosition(m_headUpperNodServo);
  frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headUpperNodServo, origPosition) );
  //origPosition = head.QueryPosition(m_headLowerNodServo);
  //frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_headLowerNodServo, origPosition) );
}
*/
	// returns  new  value
	private double checkBounds(double value) {
		if (value < -1.0) {
			value = -1.0;
		}
		if (value > 1.0) {
			value = 1.0;
		}
		return value;
	}
	private double checkChangeBounds(double value, double angle) {
		return checkChangeBoundsAndAngle(value, angle, m_maxDelta);
	}
	private double checkNodChangeBounds(double value, double angle) {
		return checkChangeBoundsAndAngle(value, angle, m_maxNodDelta);
	}
	private double checkChangeBoundsAndAngle(double value, double angle, double delta) {
		if (angle > 0.1) {
			delta *= 3;
		}
		if (value > delta) {
			value = delta;
		}
		if (value < -1.0 * delta) {
			value = -1.0 * delta;
		}
		return value;
	}
}

/*
void AttentionRule::
calculateVergence(double rightEyeDist, double angle, os_time& time,
		  WeightedBlendingMotionFrame& frame,
		  com::hansonrobotics::controlsystem::HeadControlSystem& head)
{
  return;       // No vergence for Zeno

// #ifdef USE_JOYSTICK
//   if ( m_joyStick.get() == 0 )
//     return;

//   JoystickDuctDeviceState js;

//   // Get the input's device state
//   m_joyStick->copyState( js );

//   //m_eyesVerticalPointToMoveMultiplier = js.lY / 5000.0;
//   //std::cout << "eyeVertMult: " << m_eyesVerticalPointToMoveMultiplier << std::endl;
//   if ( js.rgbButtons[5] & 0x80 && js.lZ == 1000 ) {
//     m_vergenceBaseDiff = js.lY / 1000.0;
//     std::cout << "vergence: base: " << m_vergenceBaseDiff << std::endl;
//   }
//   if ( js.rgbButtons[4] & 0x80 && js.lZ == 1000 ) {
//     m_vergenceDistMult = js.lY / 10000.0;
//     std::cout << "vergence dist: " << m_vergenceDistMult << std::endl;
//   }
// #endif

//   double OrigRightEyePosition = head.QueryPosition(m_rightEyeTurnServo);
//   double LeftEyePosition = OrigRightEyePosition + m_vergenceBaseDiff; // + rightEyeDist * m_vergenceDistMult;
//   double OrigLeftEyePosition = head.QueryPosition(m_leftEyeTurnServo);
//   double LeftEyePositionDelta = LeftEyePosition - OrigLeftEyePosition;
//   CheckNodChangeBounds(LeftEyePositionDelta, angle);
//   LeftEyePosition = OrigLeftEyePosition + LeftEyePositionDelta;

//   //std::cout << "Position: " << LeftEyePosition << std::endl;
//   //<< " Delta: " << EyesVerticalPositionDelta;

//   CheckBounds(LeftEyePosition);
//   //std::cout << "Move down to: " << EyesVerticalPosition << std::endl;
//   frame.AddMjMovement( BlendRuleWeights::ATTENTION, std::make_pair(m_leftEyeTurnServo, LeftEyePosition) );  
}
*/
