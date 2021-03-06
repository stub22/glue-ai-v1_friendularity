// startup_RTSX.sce  
// www.controlsystemslab.com   July 2012
// script file to load all RTSX functions

printf("\nRobotic Tools for Scilab/Xcos (RTSX) Version 1.00\n");
printf("by Control Systems Lab,  April, 2013\n");
printf("http://www.controlsystemslab.com/rtsx\n");

funcprot(0);  // suppress warning when function is redefined

// ============== common constants ================
pi = %pi;
false = 0;
true = 1;
// boolean
False = (1==0);
True = (1==1);
// ================= math functions ================
// quaternion
exec('./quaternion/Quaternion.sci',-1);
exec('./quaternion/q2tr.sci',-1);
exec('./quaternion/q2vec.sci',-1);
exec('./quaternion/tr2q.sci',-1);
exec('./quaternion/isquaternion.sci',-1);
exec('./quaternion/q2str.sci',-1);  // convert to string
exec('./quaternion/qinv.sci',-1);  // quaternion inversion
exec('./quaternion/qnorm.sci',-1);
exec('./quaternion/qunit.sci',-1);
exec('./quaternion/isqequal.sci',-1);
exec('./quaternion/qadd.sci',-1);      // addition/subtraction
exec('./quaternion/qmult.sci',-1);     // multiplication
exec('./quaternion/qpower.sci',-1);
exec('./quaternion/qdivide.sci',-1);
exec('./quaternion/qinterp.sci',-1);
exec('./quaternion/qscale.sci',-1);

// ===============rotation matrices================
// basic rotation matrices
exec('rotx.sci',-1);
exec('roty.sci',-1);
exec('rotz.sci',-1);
// data conversions
exec('eul2r.sci',-1);    // euler angles to rotation matrix R
exec('rpy2r.sci',-1);    // RPY angles to R
exec('angvec2r.sci',-1);    // angle/vector to R
exec('rotxyz2r.sci',-1);  // axis X,Y,Z rotations to R

// ================= homogeneous transformation ===========
// rotation
exec('trotx.sci',-1);
exec('troty.sci',-1);
exec('trotz.sci',-1);
exec('trotw.sci',-1);  // rotation about a world coordinate axis
// translation
exec('transl.sci',-1);
exec('mtransl.sci',-1);    // translation sequences 
// data conversions
exec('t2p.sci',-1);
exec('t2r.sci',-1);
exec('r2t.sci',-1);
exec('rp2t.sci',-1);
exec('eul2t.sci',-1);
exec('rpy2t.sci',-1);
exec('tr2eul.sci',-1);
exec('tr2angvec.sci',-1);
exec('angvec2t.sci',-1);
exec('tr2rpy.sci',-1);
exec('gentrot.sci',-1); // generate sequence of basic rotation T
// frame construction and manipulation
exec('Frame.sci',-1);
exec('SerialFrame.sci',-1);
exec('InsertFrame.sci',-1);
exec('DeleteFrame.sci',-1);
exec('ReplaceFrame.sci',-1);
exec('ResolveFrame.sci',-1);
// utilities
exec('trnorm.sci',-1);
// ============ robot modeling and forward/inverse kinematics =====================
exec('Link.sci',-1);
exec('AppendLink.sci',-1);    // append a robot link
exec('RemoveLink.sci',-1);    // remove a robot link
exec('ReplaceLink.sci',-1);    // replace a robot link
exec('AttachBase.sci',-1);
exec('AttachTool.sci',-1);
exec('DetachBase.sci',-1);
exec('DetachTool.sci',-1);
exec('Link2AT.sci',-1);
exec('Robot2AT.sci',-1);    // added Nov 2012
exec('SerialLink.sci',-1);
exec('Robotinfo.sci',-1);
exec('FKine.sci',-1);
exec('Robot2hAT.sci',-1);
exec('UpdateRobot.sci',-1);
exec('UpdateRobotLink.sci',-1);
exec('IKine6s.sci',-1);
exec('ikine.sci',-1);

// ======================= velocity kinematics ============================
exec('tr2delta.sci',-1);
exec('delta2tr.sci',-1);
exec('tr2jac.sci',-1);
exec('jacobn.sci',-1);
exec('jacob0.sci',-1);
exec('rpy2jac.sci',-1);
exec('eul2jac.sci',-1);
exec('maniplty.sci',-1);    // added Nov 2012

// ======================== plotting/amimation functions ==================

exec('trplot.sci',-1);
exec('tranimate.sci',-1);
exec('PlotFrame.sci',-1);
exec('PlotResolveFrame.sci',-1);
exec('PlotRobot.sci',-1);
exec('PlotRobotFrame.sci',-1);
exec('AnimateRobot.sci',-1);
exec('AnimateRobotFrame.sci',-1);

// =================== path generation ==================
exec('cpoly.sci',-1);  // cubic polynomial
exec('qpoly.sci',-1);  // quintic polynomial
exec('lspb.sci',-1);   // linear segment parabolic blend
exec('mtraj.sci',-1);
exec('mstraj.sci',-1);
exec('jtraj.sci',-1);
exec('trinterp.sci',-1);
exec('ctraj.sci',-1);

// ================= dynamics ========================
exec('friction.sci',-1);
exec('rne.sci',-1);
exec('rne_dh.sci',-1);
exec('gravload.sci',-1);
exec('inertia.sci',-1);
exec('coriolis.sci',-1);
exec('payload.sci',-1);
exec('accel.sci',-1);

// ================ Machine vision ==============================
// ============  first added April, 2013 =============================
exec('./vision/CentralCamera.sci',-1);
exec('./vision/CamProject.sci',-1);
exec('./vision/CamPlot.sci',-1);
exec('./vision/mkgrid.sci',-1);
exec('./vision/utilities.sci',-1);
exec('./vision/visjac_p.sci',-1);
exec('./vision/IBVS4.sci',-1);
exec('./vision/depth_estimator.sci',-1);

// ==================== graphic functions ========================
exec('./graphics/GeoPatMakeBlock.sci',-1);  // make patches for rectangular block
exec('./graphics/GeoVerMakeBlock.sci',-1); // make vertices of rectangular block
exec('./graphics/GeoPatMakeCylinder.sci',-1);
exec('./graphics/GeoVerMakeCylinder.sci',-1);
exec('./graphics/GeoMakeJointAxis.sci',-1);
exec('./graphics/GeoMakeFrame.sci',-1);
exec('./graphics/SetViewAngle.sci',-1);
exec('./graphics/GetViewAngle.sci',-1);
exec('./graphics/colormap.sci',-1);
exec('./graphics/linesmap.sci',-1);
exec('./graphics/ellipsoid.sci',-1);   // plot ellipsoid, added Nov 10,2012
exec('./graphics/plot_ellipse.sci',-1);
// =================== common functions ==========================
exec('./common/numrows.sci',-1);
exec('./common/numcols.sci',-1);
exec('./common/hold.sci',-1);
exec('./common/linecenter.sci',-1);
exec('./common/unit.sci',-1);
exec('./common/isrot.sci',-1);
exec('./common/isvec.sci',-1);
exec('./common/ishomog.sci',-1);
exec('./common/Xlocate.sci',-1);  // locate intesection of X_i and Z_i-1
exec('./common/Xlocate4.sci',-1); // modified for 4-D A and T matrices
exec('./common/isspherical.sci',-1);  // test spherical wrist
exec('./common/vex.sci',-1);
exec('./common/skew.sci',-1);
exec('./common/blkdiag2.sci',-1);
exec('./common/isscalar.sci',-1);
exec('./common/polyval.sci',-1);
exec('./common/_zeros_ones.sci',-1);
exec('./common/unitvec.sci',-1);   // computes unit vector
exec('./common/display.sci',-1);
exec('./common/cross.sci',-1);  // cross product
// ============= demo ===============
exec('./demo/rprdemo.sci',-1);






















