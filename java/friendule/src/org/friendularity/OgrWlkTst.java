package org.friendularity;

import com.jme3.animation.AnimChannel;
import com.jme3.animation.AnimControl;
import com.jme3.animation.AnimEventListener;
import com.jme3.animation.Bone;
import com.jme3.animation.LoopMode;
import com.jme3.app.SimpleApplication;
import com.jme3.input.KeyInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.light.DirectionalLight;
import com.jme3.material.Material;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.scene.debug.SkeletonDebugger;


/**
 *
 * @author winston
 */
public class OgrWlkTst extends SimpleApplication implements AnimEventListener {

  private AnimChannel channel;
  private AnimControl control;
  Node player;

    private float myWaistTwistAngle = 0;
    private float myWaistTwistRate = 1;

  public static void main(String[] args) {
    OgrWlkTst owTst = new OgrWlkTst();
    owTst.start();
  }

  @Override
  public void simpleInitApp() {
    viewPort.setBackgroundColor(ColorRGBA.LightGray);
    initKeys();

    // JME2-only so far, it seems
   // JoystickInput.setProvider( InputSystem.INPUT_SYSTEM_LWJGL );

    DirectionalLight dl = new DirectionalLight();
    dl.setDirection(new Vector3f(-0.1f, -1f, -1).normalizeLocal());
    rootNode.addLight(dl);

    /*
     * These mesh.xml files are excluded from assets.jar in JME default setup
     * (see "assets" section of project properties)
     */
     // excludes: */*.mesh.xml,**/*.skeleton.xml,**/*.scene,**/*.material,**/*.meshxml,**/*.skeletonxml,**/*.obj,**/*.mtl

    player = (Node) assetManager.loadModel("Models/OtoSamp/Oto.mesh.xml");
    player.setLocalScale(0.5f);
    rootNode.attachChild(player);

    control = player.getControl(AnimControl.class);
    control.addListener(this);
    channel = control.createChannel();
    channel.setAnim("stand");
    // Dodge, push, pull, Walk, Stand

     SkeletonDebugger skeletonDebug = new SkeletonDebugger("skeleton", control.getSkeleton());
     Material mat = new Material(assetManager, "Common/MatDefs/Misc/WireColor.j3md");
     mat.setColor("m_Color", ColorRGBA.Green);
     mat.getAdditionalRenderState().setDepthTest(false);
     skeletonDebug.setMaterial(mat);
     player.attachChild(skeletonDebug);

  }

  public void onAnimCycleDone(AnimControl control, AnimChannel channel, String animName) {
    if (animName.equals("Walk") || animName.equals("Dodge")) {
      channel.setAnim("stand", 0.50f);
      channel.setLoopMode(LoopMode.DontLoop);
      channel.setSpeed(1f);
    }
  }

  public void onAnimChange(AnimControl control, AnimChannel channel, String animName) {
    // unused
  }

  /** Custom Keybinding: Map named actions to inputs. */
  private void initKeys() {
    inputManager.addMapping("Walk", new KeyTrigger(KeyInput.KEY_SPACE));
    inputManager.addListener(walkAL, "Walk");
    inputManager.addMapping("Dodge", new KeyTrigger(KeyInput.KEY_K));
    inputManager.addMapping("push", new KeyTrigger(KeyInput.KEY_J));
    inputManager.addMapping("pull", new KeyTrigger(KeyInput.KEY_L));
    inputManager.addListener(playAL, "pull");
    inputManager.addListener(playAL, "push");
    inputManager.addListener(playAL, "Dodge");
  }

  private ActionListener walkAL = new ActionListener() {
    public void onAction(String name, boolean keyPressed, float tpf) {
      if (name.equals("Walk") && !keyPressed) {
        if (!channel.getAnimationName().equals("Walk")) {
          channel.setAnim("Walk", 0.50f);
          channel.setLoopMode(LoopMode.Loop);
        }
      }
    }
  };
  private ActionListener playAL = new ActionListener() {
    public void onAction(String name, boolean keyPressed, float tpf) {
        System.out.println("playAL got name: " + name + "keyP=" + keyPressed);
      if (// name.equals("Dodge") &&
             !keyPressed) {
        if (!channel.getAnimationName().equals(name)) {
          channel.setAnim(name, 0.50f);
          channel.setLoopMode(LoopMode.DontLoop);
        }
      }
    }
  };

    @Override
    public void simpleUpdate(float tpf){
        Bone b = control.getSkeleton().getBone("spinehigh");

        myWaistTwistAngle += tpf * myWaistTwistRate;
        if (myWaistTwistAngle > FastMath.HALF_PI / 2f){
            myWaistTwistAngle = FastMath.HALF_PI / 2f;
            myWaistTwistRate = -1;
        } else if (myWaistTwistAngle < -FastMath.HALF_PI / 2f){
            myWaistTwistAngle = -FastMath.HALF_PI / 2f;
            myWaistTwistRate = 1;
        }

        Quaternion q = new Quaternion();
        // yaw, roll, pitch
        q.fromAngles(0, myWaistTwistAngle, 0);

        b.setUserControl(true);
        b.setUserTransforms(Vector3f.ZERO, q, Vector3f.UNIT_XYZ);

        Bone bal = control.getSkeleton().getBone("arm.left");

        Quaternion q2 = new Quaternion();
        // yaw, roll, pitch
        q2.fromAngles(myWaistTwistAngle, 0, 0);

        bal.setUserControl(true);
        bal.setUserTransforms(Vector3f.ZERO, q2, Vector3f.UNIT_XYZ);

        Bone upr = control.getSkeleton().getBone("uparm.right");
        upr.setUserControl(true);
        upr.setUserTransforms(Vector3f.ZERO, q2, Vector3f.UNIT_XYZ);
    }
}
