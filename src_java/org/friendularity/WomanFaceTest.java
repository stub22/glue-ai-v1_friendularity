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

import com.jme3.animation.AnimChannel;
import com.jme3.animation.AnimControl;
import com.jme3.animation.AnimEventListener;
import com.jme3.animation.Bone;
import com.jme3.animation.LoopMode;
import com.jme3.animation.Skeleton;
import com.jme3.app.SimpleApplication;
import com.jme3.font.BitmapFont;
import com.jme3.font.BitmapText;
import com.jme3.font.Rectangle;
import com.jme3.input.FlyByCamera;
import com.jme3.light.DirectionalLight;
import com.jme3.math.Vector3f;
import com.jme3.scene.Node;
import com.jme3.math.ColorRGBA;
import com.jme3.math.FastMath;
import com.jme3.math.Quaternion;
import com.jme3.renderer.Camera;
import com.jme3.renderer.ViewPort;
import com.jme3.system.AppSettings;
import com.jme3.system.JmeCanvasContext;
import com.jme3.system.JmeContext;
import java.awt.BorderLayout;
import java.awt.Canvas;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import javax.swing.JFrame;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class WomanFaceTest extends SimpleApplication implements AnimEventListener {

	private AnimChannel		channel;
	List<AnimControl>		myAnimControls;
	// Node otoNode;
	private float			myWaistTwistAngle = 0;
	private float			myWaistTwistRate = 1;
	private BitmapText		myTextBox;
	private VirtCharPanel	myVCP;

	public static void main(String[] args) {
		WomanFaceTest owTst = new WomanFaceTest();
		owTst.startCanvasInPanelInFrame();
		// owTst.start(JmeContext.Type.Display);
	}
	public List<AnimControl>	getAnimControls() {
		return myAnimControls;
	}
	public Canvas makeCanvas() {
		/* See Jmonkey examples    "TestCanvas.java" and "AppHarness.java"  */
		AppSettings settings = new AppSettings(true);
		int width = 640;
		int height = 480;
		settings.setWidth(width);
		settings.setHeight(height);
		// settings.setUseInput(false);
		setSettings(settings);
        createCanvas();
/* In a silent applet, we might do:
 *         settings.setAudioRenderer(null);
        JmeSystem.setLowPermissions(true);
 */
		JmeContext ctx = getContext();
		JmeCanvasContext cctx = (JmeCanvasContext) ctx;
		Canvas awtCanvas = cctx.getCanvas();
        awtCanvas.setSize(width, height);
		return awtCanvas;
	}



	public void startCanvasInPanelInFrame() {
		Canvas c = makeCanvas();
		myVCP  = new VirtCharPanel();
		myVCP.setRenderCanvas(c);

		JFrame frame = new JFrame(this.getClass().getName());
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(myVCP, BorderLayout.CENTER);

		frame.pack();

		frame.setVisible(true);
		startCanvas();  		// equivalent to this?:     start(JmeContext.Type.Canvas);

		Future fut = enqueue(new Callable() {
			public Object call() throws Exception {
				System.out.println("*********** Enqueued call is executing");
				return null;
			}
		});
//to retrieve return value (waits for call to finish, fire&forget otherwise):
		Object unusedResult;
		try {
			unusedResult = fut.get();
		} catch (Throwable t) {
			t.printStackTrace();
		}
		System.out.println("*********** startCanvasInPanelInFrame is returning");
	}

	@Override public void simpleInitApp() {
		System.out.println("*********** SimpleInitApp is starting");

		FlyByCamera fbc = getFlyByCamera();
		fbc.setMoveSpeed(10f);
		setPauseOnLostFocus(false);
		ViewPort vp = getViewPort();
		vp.setBackgroundColor(ColorRGBA.LightGray);
//    initKeys();

		// JME2-only so far, it seems
		// JoystickInput.setProvider( InputSystem.INPUT_SYSTEM_LWJGL );

		DirectionalLight dl = new DirectionalLight();
		dl.setDirection(new Vector3f(-0.4f, -0.5f, -0.5f).normalizeLocal());
		rootNode.addLight(dl);

		Camera cam = getCamera();

		Quaternion camRotQ = new Quaternion(0.0f, 1.0f, 0.5f, 0.0f);
		cam.setAxes(camRotQ);


		// cam.se

		/*
		 * These mesh.xml files are excluded from assets.jar in JME default setup
		 * (see "assets" section of project properties)
		 */
		// excludes: */*.mesh.xml,**/*.skeleton.xml,**/*.scene,**/*.material,**/*.meshxml,**/*.skeletonxml,**/*.obj,**/*.mtl
/*
		otoNode = (Node) assetManager.loadModel("Models/OtoSamp/Oto.mesh.xml");
		otoNode.setLocalScale(0.5f);
		rootNode.attachChild(otoNode);

		control = otoNode.getControl(AnimControl.class);
		control.addListener(this);
		channel = control.createChannel();
		channel.setAnim("stand");
		// Dodge, push, pull, Walk, Stand

		SkeletonDebugger skeletonDebug = new SkeletonDebugger("skeleton", control.getSkeleton());
		Material mat = new Material(assetManager, "Common/MatDefs/Misc/WireColor.j3md");
		mat.setColor("m_Color", ColorRGBA.Green);
		mat.getAdditionalRenderState().setDepthTest(false);
		skeletonDebug.setMaterial(mat);
		otoNode.attachChild(skeletonDebug);
		 */
		initWomanFaceModel();
		setupTextBox();
		System.out.println("*********** SimpleInitApp is finished");

	}

	public void initWomanFaceModel() {
		// Node womanFaceNode = (Node) assetManager.loadModel("resources/womanTest_20110218/faceShape.mesh.xml");
	  /*
		System.out.println("Loading test1Node");
		Node test1Node = (Node) assetManager.loadModel("resources/leo_hanson_tests/test1/test1_groShape.mesh.xml");
		System.out.println("test1_groShape loaded: " + test1Node);

		test1Node.setLocalScale(0.5f);
		rootNode.attachChild(test1Node);
		Node test2HeadNode = (Node) assetManager.loadModel("resources/leo_hanson_tests/test2/headShape.mesh.xml");
		test2HeadNode.setLocalScale(0.5f);
		rootNode.attachChild(test2HeadNode);
		 */
		// Node testSceneNode = (Node) assetManager.loadModel("resources/leo_hanson_tests/test1/test1.scene");
		// Node testSceneNode = (Node) assetManager.loadModel("resources/leo_hanson_tests/test2/test2.scene");
		// Node testSceneNode = (Node) assetManager.loadModel("resources/leo_hanson_tests/test3/test3.scene");
		Node testSceneNode = (Node) assetManager.loadModel("resources/stu_stickbot_01/ss.scene");
		System.out.println("test scene loaded: " + testSceneNode);

		SpatialManipFuncs.dumpNodeTree(testSceneNode, "   ");
		myAnimControls = SpatialManipFuncs.findAnimControls(testSceneNode);

		System.out.println("Found animControls " + myAnimControls);

		for (AnimControl ac : myAnimControls) {
			Skeleton csk = ac.getSkeleton();
			csk.reset();  		// Forces bones to absorb default-bind-pose coords from nodes.
			Bone roots[] = csk.getRoots();
			Bone rb = roots[0];
			Vector3f localPos = rb.getLocalPosition();
			Vector3f modelPos = rb.getModelSpacePosition();
			System.out.println("root bone=" + rb + ", localPos=" + localPos + ", modelPos=" + modelPos);
			List<Bone> bkl = rb.getChildren();
			for (Bone bk : bkl) {
				System.out.println("child bone=" + bk + ", localPos=" + bk.getLocalPosition() + ", modelPos=" + bk.getModelSpacePosition());
			}
		}
		// System.out.println("================================================================");
		
		/*
		 * found sceneKid spatial: body-node (Node)
		found sceneKid spatial: rightArm-node (Node)
		java.lang.NullPointerException
		found sceneKid spatial: leftArm-node (Node)
		found sceneKid spatial: head-node (Node)
		 */


/*
		Node headNode = (Node) testSceneNode.getChild("head-node");
		System.out.println("head-node: " + headNode);
		Node headShapeNode = (Node) headNode.getChild("headShape-entity");
		System.out.println("headShape-entity: " + headShapeNode);
 *
 */
		// myAnimControl = headShapeNode.getControl(AnimControl.class);


		// Material testSceneMat = new Material(assetManager, "resources/leo_hanson_tests/test3/test3.material");

		testSceneNode.setLocalScale(0.5f);
		rootNode.attachChild(testSceneNode);

		/*
		if (myAnimControl == null) {
		throw new RuntimeException("cant lookup animControl in sceneNode");
		}
		 */

	}

	public void setupTextBox() {

		BitmapFont fnt = assetManager.loadFont("Interface/Fonts/Default.fnt");
		myTextBox = new BitmapText(fnt, false);
		myTextBox.setBox(new Rectangle(0, 0, settings.getWidth(), settings.getHeight()));
		myTextBox.setSize(fnt.getPreferredSize() * 2f);
		myTextBox.setText("nothing to say here, just yet");
		myTextBox.setLocalTranslation(0, settings.getHeight(), 0);
		guiNode.attachChild(myTextBox);
	}
	/*
	public void dumpScene(Node sceneNode) {
	sceneNode.depthFirstTraversal(new SceneGraphVisitor() {

	});
	}
	 */

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

	@Override
	public void simpleUpdate(float tpf) {
		//	System.out.println("simpleUpdate, tpf=" + tpf);
		int testChannelNum = myVCP.getTestChannelNum();

		String direction = myVCP.getTestDirection();
		// System.out.println("Direction=" + direction);
		AnimControl ac = myAnimControls.get(testChannelNum);
		Skeleton csk = ac.getSkeleton();

		Bone roots[] = csk.getRoots();
		// System.out.println("Found " + roots.length + " root bones: " + roots);
		Bone rootBone = roots[0];
		Bone tgtBone = rootBone;
		String mod = myVCP.getTestChannelModifier();
		if (mod.equals("first child")) {
			List<Bone> kids = rootBone.getChildren();
			Bone firstKid = kids.get(0);
			tgtBone = firstKid;
		}

		Vector3f localPos = rootBone.getLocalPosition();
		Vector3f modelPos = rootBone.getModelSpacePosition();
		// System.out.println("================================================================");
		myVCP.setDumpText("tgtBone=" + tgtBone + ", localPos=" + localPos + ", modelPos=" + modelPos);


		myWaistTwistAngle += tpf * myWaistTwistRate;
		if (myWaistTwistAngle > FastMath.HALF_PI / 2f) {
			myWaistTwistAngle = FastMath.HALF_PI / 2f;
			myWaistTwistRate = -1;
		} else if (myWaistTwistAngle < -FastMath.HALF_PI / 2f) {
			myWaistTwistAngle = -FastMath.HALF_PI / 2f;
			myWaistTwistRate = 1;
		}

		Quaternion q = new Quaternion();
		// yaw, roll, pitch
		//	System.out.println("Setting roll for bone: " + b + " to " + myWaistTwistAngle);

		float pitchAngle = 0.0f;
		float rollAngle = 0.0f;
		float yawAngle = 0.0f;
		if (direction.equals("pitch")) {
			pitchAngle = myWaistTwistAngle;
		} else if (direction.equals("roll")) {
			rollAngle = myWaistTwistAngle;
		} else if (direction.equals("yaw")) {
			yawAngle = myWaistTwistAngle;
		}
		q.fromAngles(pitchAngle, rollAngle, yawAngle);
		tgtBone.setUserControl(true);
		tgtBone.setUserTransforms(Vector3f.ZERO, q, Vector3f.UNIT_XYZ);

	}
}
