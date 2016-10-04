package org.friendularity.vwimpl

import java.awt.Canvas
import java.awt.event.{WindowEvent, WindowAdapter, WindowListener}
import javax.swing.JFrame

import com.jme3.app.SimpleApplication
import com.jme3.system.{JmeCanvasContext, JmeContext}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.blob.emit.RenderConfigEmitter
import org.cogchar.render.gui.bony.{PanelUtils, VirtualCharacterPanel}
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.cpmsg.CPMsgTeller

/**
  * Created by Stub22 on 9/6/2016.
  */

class WrappedSimBalloonApp extends  SimBalloonJmeApp {
	def doLegacyApplySettings : Unit = {
		applySettings()
	}
	def doHideJmeDebug : Unit = {
		hideJmonkeyDebugInfo()
	}
}

class SimBalloonWindowEventAdapter() extends WindowAdapter with VarargsLogging {
	override def windowClosed(e: WindowEvent): Unit = {
		super.windowClosed(e)
		warn1("Got WindowClosed event: {}", e)

	}
}

trait VWJdkAwtCanvasMgr extends VarargsLogging {

	def launch(resultsTeller : CPMsgTeller) : Unit = {
		val sbJmeApp: WrappedSimBalloonApp = makeJmeApp

		sbJmeApp.wireSetupResultsTeller(resultsTeller)

		makePanelForApp(sbJmeApp)

		info0("VWJdkSwingCanvasLauncher.launch calling sbJmeApp.start")

		val flag_blockUntilRunning = true

		// assetManager does not exist until start is called, triggering simpleInit callback.

		sbJmeApp.startCanvas(flag_blockUntilRunning)

		info0("VWJdkSwingCanvasLauncher.launch - END")
	}
	def makeJmeApp : WrappedSimBalloonApp = {
		new WrappedSimBalloonApp
	}
	def makePanelForApp(app : WrappedSimBalloonApp) : Unit = {
		val bce = new RenderConfigEmitter();
		val panelKind = "SLIM"
		info1("******************* Initializing VirtualCharacterPanel of kind={} with canvas", panelKind);
		val vcp : VirtualCharacterPanel  = PanelUtils.makeVCPanel(bce, panelKind);
		val can = makeAWTCanvas(app, 600, 400)
		vcp.setRenderCanvas(can)
		// Old step no longer needed:		getBonyRenderContext().setPanel(vcp);

		// Frame must be packed after panel created, but created before startCanvas.
		// Additional ancient comment:
		// If startJMonkey is called first, we often hang in frame.setVisible() as JMonkey tries
		// to do some magic restart deal that doesn't work as of jme3-alpha4-August_2011.
		val jf : JFrame  = vcp.makeEnclosingJFrame("VWJdk Swing Canvas");

		// Another old comment, related to OSGi shutdown:
		// Frame will receive a close event when org.cogchar.bundle.render.opengl is STOPPED
		// So, that's our attempt to close the window gracefully on app exit (under OSGi).

		// Meanwhile, if someone X-s the window, the optWindowEventListener gets a callback,
		// which could try to shut down whatever system is running (e.g. OSGi).
		val winEvtAdapter = new SimBalloonWindowEventAdapter

		jf.addWindowListener(winEvtAdapter)
	}

	def makeAWTCanvas(app : WrappedSimBalloonApp, width : Int, height : Int)  : Canvas = {

		app.doLegacyApplySettings // Works if called now, but not after createCanvas.
		app.doHideJmeDebug

		app.createCanvas()
		// Does not work at this time or subsq:		app.doLegacyApplySettings

		val  ctx : JmeContext = app.getContext();

		val cctx : JmeCanvasContext = ctx.asInstanceOf[JmeCanvasContext];
		val awtCanvas : Canvas = cctx.getCanvas();
		awtCanvas.setSize(width, height);
		return awtCanvas;
	}
		/****
	*theDbg.logInfo("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Value of org.lwjgl.librarypath = " + System.getProperty("org.lwjgl.librarypath"));
	*theDbg.logInfo("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Forcing lowPermissions during initCharPanelWithCanvas(), to prevent JME3 forcing value into org.lwjgl.librarypath");

	*JmeSystem.setLowPermissions(true);
	*bvcApp.initCharPanelWithCanvas(vcp);
	*JmeSystem.setLowPermissions(false);

	*resultBRC = bvcApp.getBonyRenderContext();
	*theDbg.logInfo("******************* Registering BonyRenderContext as OSGi service");
	*bundleCtx.registerService(BonyRenderContext.class.getName(), resultBRC, null);
	*return resultBRC;
*****/
}
trait WinStatusAdapter extends WindowStatusMonitor {  }

/*
	public String getPanelKind() {
		return "SLIM";
	}
 */

/**********************************
*From Cogchar: VWorldRegistry.java
  *public void initVWorldUnsafe(PumaContextMediator mediator) throws Throwable {
  *String panelKind = mediator.getPanelKind();
  *

  *PumaVirtualWorldMapper vwm = getVWM();
  *HumanoidRenderContext hrc = vwm.initHumanoidRenderContext(panelKind);
  *boolean allowJFrames = mediator.getFlagAllowJFrames();
  *if (allowJFrames) {
  *
  *WindowAdapter winLis = new WindowAdapter() {
 *
  *@Override
                public void windowClosed(WindowEvent e) {
                    getLogger().warn("PumaBooter caught window CLOSED event for OpenGL frame:  {}", e);
                    notifyVWorldWindowClosed();
                }
            };
            getLogger().debug("%%%%%%%%%%%%%%%%%%% Calling startOpenGLCanvas");
            vwm.startOpenGLCanvas(allowJFrames, winLis);

        }

        hrc.runPostInitLaunchOnJmeThread();
    }

-------------------
PumaVirtualWorldMapper.java
    public HumanoidRenderContext initHumanoidRenderContext(String panelKind) {
        BundleContext bundleCtx = OSGiUtils.getBundleContext(HumanoidRenderContext.class);
        if (bundleCtx == null) {
            return null;
        }
        myCRC = (HumanoidRenderContext) RenderBundleUtils.buildBonyRenderContextInOSGi(bundleCtx,
        		panelKind);

        return (HumanoidRenderContext) myCRC;
    }

	public void startOpenGLCanvas(boolean wrapInJFrameFlag, java.awt.event.WindowListener optWinLis) throws Exception {
			HumanoidRenderContext hrc = getHumanoidRenderContext();
			if (hrc != null) {
				hrc.startOpenGLCanvas(wrapInJFrameFlag, optWinLis);


-----------------------
From HumanoidRenderContext.java:

     * Second (and most crucial) stage of OpenGL init. This method blocks until
     * the canvas initialization is complete, which requires that the simpleInitApp() methods have all completed.

public void startOpenGLCanvas(boolean wrapInJFrameFlag, WindowListener optWindowEventListener) throws Exception {

	if (wrapInJFrameFlag) {
	VirtualCharacterPanel vcp = getPanel();
	logInfo("Making enclosing JFrame for VirtCharPanel: " + vcp);
	// Frame must be packed after panel created, but created  before startJMonkey.
	// If startJMonkey is called first, we often hang in frame.setVisible() as JMonkey tries
	// to do some magic restart deal that doesn't work as of jme3-alpha4-August_2011.

	// During the Frame-pack portion of this method, we get all the way to:
	//  CogcharPresumedApp - ********************* DemoApp.initialize() called
	JFrame jf = vcp.makeEnclosingJFrame("CCRK-PUMA Virtual World");
	logInfo("Got Enclosing Frame, adding to BonyRenderContext for WindowClose triggering: " + jf);
	// Frame will receive a close event when org.cogchar.bundle.render.opengl is STOPPED
	// So, that's our attempt to close the window gracefully on app exit (under OSGi).
	setFrame(jf);
	// Meanwhile, if someone X-s the window, the optWindowEventListener gets a callback,
	// which could try to shut down whatever system is running (e.g. OSGi).
	if (optWindowEventListener != null) {
	jf.addWindowListener(optWindowEventListener);
}
}
	BonyVirtualCharApp app = getApp();

	if (app.isCanvasStarted()) {
	logWarning("JMonkey Canvas was already started!");
} else {

	logInfo("Starting JMonkey canvas - hold yer breath! [[[[[[[[[[[[[[[[[[[[[[[[[[");
	app.startJMonkeyCanvas();
	logInfo("]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]  Finished starting JMonkey canvas!");
}
}
-----------
setFrame is just a setter in
BonyRenderContext

	public void setFrame(JFrame jf) {
		myFrame = jf;
	}
	public JFrame getFrame() {
		return myFrame;
	}
	public VirtualCharacterPanel getPanel() {
		return myPanel;
	}
	public void setPanel(VirtualCharacterPanel panel) {
		this.myPanel = panel;
		setWindowStatusReader(panel);
	}
---------
Lower down in FramedRenderContext

public class FramedRenderContext extends CogcharRenderContext implements WindowStatusMonitor {
	private Dimension myStoredWindowSize = null;
	private WindowStatusReader	myReader = null;
	private Set<WantsWindowStatus> myListeners = new HashSet<WantsWindowStatus>();

	public FramedRenderContext(RenderRegistryClient rrc) {
		super(rrc);
	}

	@Override public Dimension getWindowSize() {
		return myStoredWindowSize;
	}

	@Override public void addListener(WantsWindowStatus wws) {
		myListeners.add(wws);
	}
	public void storeWindowSize(Dimension ws) {
		myStoredWindowSize = ws;
		for(WantsWindowStatus listener : myListeners) {
			listener.notifyWindowSize(ws);
		}
	}
	private Dimension myCurrSizeBuffer = null;
	@Override public void doUpdate(float tpf) {
		if (myReader != null) {
			myCurrSizeBuffer = myReader.getSize(myCurrSizeBuffer);
			if (myCurrSizeBuffer != null) {
				if ((myStoredWindowSize == null) || !myStoredWindowSize.equals(myCurrSizeBuffer)) {
					Dimension copiedVal = (Dimension) myCurrSizeBuffer.clone();
					storeWindowSize(copiedVal);
				}
			}
		}
		super.doUpdate(tpf);
	}

	public void setWindowStatusReader(WindowStatusReader wsr) {
		myReader = wsr;
	}
}

----------
public class BasicGoodyCtxImpl extends BasicDebugger implements BasicGoodyCtx, WantsWindowStatus {
	public BasicGoodyCtxImpl(RenderRegistryClient rrc, WindowStatusMonitor wsm) {//  GoodyModularRenderContext gmrc) {
		myRRC = rrc;
		// attachRootGoodyNode();    Removed 2016-09-03 by stub22
		wsm.addListener(this);
		Dimension winSzOrNull = wsm.getWindowSize();
		if (winSzOrNull != null) {
			getLogger().info("Found initial window size, applying: {}", winSzOrNull);
			applyNewScreenDimension(winSzOrNull);
		} else {
			getLogger().warn("No initial window size found");
		}
	}
	@Override public void applyNewScreenDimension(Dimension newDimension) {
		myScreenDimension = newDimension;
		// Notify goodies of new dimensions
		for (VWorldEntity aGoody : myVWER.getAllGoodies()) {
			aGoody.applyScreenDimension(myScreenDimension);
		}
	}
------------------
	public void initCharPanelWithCanvas(VirtualCharacterPanel vcp) {
		// Works
		applySettings();
		hideJmonkeyDebugInfo();
		this.createCanvas();
		// Does not work at this time or subsq:
		//applySettings();
		Canvas c = WorkaroundFuncsMustDie.makeAWTCanvas(this);
		vcp.setRenderCanvas(c);
		getBonyRenderContext().setPanel(vcp);
		// assetManager does not exist until start is called, triggering simpleInit callback.
	}

	 * Blocks until all canvas init is complete, including execution of the simpleInitApp methods.

	public void startJMonkeyCanvas() {
		theLogger.info("*********** startJMonkeyCanvas is starting");
		this.startCanvas();  		// equivalent to this?:     start(JmeContext.Type.Canvas);

		Future fut = enqueue(new Callable() {
			public Object call() throws Exception {
				theLogger.info("*********** Enqueued call is executing");
				return null;
			}
		});
		theLogger.info("*********** startJMonkeyCanvas is waiting for the future to come");
//to retrieve return value (waits for call to finish, fire&forget otherwise):
		Object unusedResult;
		try {
			unusedResult = fut.get();
		} catch (Throwable t) {
			t.printStackTrace();
		}
		theLogger.info("*********** startJMonkeyCanvas sees that the future has arrived, so it is returning");
        myCanvasStartedFlag = true;
	}

    public boolean isCanvasStarted(){
        return myCanvasStartedFlag;
    }
	// This will get called during the startJMonkeyCanvas invocation above, *or* during any alternative
	// App-start invoked from a test harness, e.g. GoodyRenderTestApp.
	@Override public void simpleInitApp() {
		theLogger.info("*********** BonyVirtualCharApp.simpleInitApp() is starting");
		super.simpleInitApp();
		theLogger.info("*********** BonyVirtualCharApp.simpleInitApp() is finished - JME3 infrastructre is ready.");
	}

---------
	OSGi render context is made in
RenderBundleUtils.java   (for standalone app see HumanoidPuppetTestMain)

	public static BonyRenderContext buildBonyRenderContextInOSGi(BundleContext bundleCtx, String panelKind) {
		BonyRenderContext	resultBRC;

		// IDE hints may show these symbols (from transitive deps) as undefined, but they should compile OK with maven.
		theDbg.logInfo("******************* Fetching VerySimpleRegistry");

*/
		/*
		theLogger.info("******************* Registering assumed resource bundle with default AssetContext");
		AssetContext defAssetCtx = RenderRegistryFuncs.findOrMakeAssetContext(null, null);
		JmonkeyAssetLocation jmal = new JmonkeyAssetLocation(ResourceBundleActivator.class);
		defAssetCtx.addAssetSource(null);
		 *
		 */
/*
		theDbg.logInfo("******************* Creating BonyConfigEmitter, HumanoidPuppetApp");
		// TODO - lookup the sysContextURI from appropriate place.
		RenderConfigEmitter bce = new RenderConfigEmitter();
		BonyVirtualCharApp bvcApp = new HumanoidPuppetApp(bce);

		// Want to decide this "kind" based on further context (e.g. "are we in Netbeans?"  "are we in debug-mode?"),
		// which is a concrete reason to try to push this init out of the bundle activator, and perform on demand
		// instead.

		theDbg.logInfo("******************* Initializing VirtualCharacterPanel of kind " + panelKind + " with canvas");

		VirtualCharacterPanel vcp = PanelUtils.makeVCPanel(bce, panelKind);
		theDbg.logInfo("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Value of org.lwjgl.librarypath = " + System.getProperty("org.lwjgl.librarypath"));
		theDbg.logInfo("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Forcing lowPermissions during initCharPanelWithCanvas(), to prevent JME3 forcing value into org.lwjgl.librarypath");

		JmeSystem.setLowPermissions(true);
		bvcApp.initCharPanelWithCanvas(vcp);
		JmeSystem.setLowPermissions(false);

		resultBRC = bvcApp.getBonyRenderContext();
		theDbg.logInfo("******************* Registering BonyRenderContext as OSGi service");
		bundleCtx.registerService(BonyRenderContext.class.getName(), resultBRC, null);
		return resultBRC;
	}
	public static void shutdownBonyRenderContextInOSGi(BundleContext bundleCtx) {
				// Attempt to cleanup OpenGL resources, which happens nicely in standalone demo if the window is X-ed.
		// (Probably cleanup is happening during dispose(), so direct call to that should work too).
		BonyRenderContext bc = getBonyRenderContext(bundleCtx);
		if (bc != null) {
			JFrame jf = bc.getFrame();
			if (jf != null) {
				theDbg.logInfo("Sending WINDOW_CLOSING event to BonyRenderContext.JFrame");
				WindowEvent windowClosing = new WindowEvent(jf, WindowEvent.WINDOW_CLOSING);
				jf.dispatchEvent(windowClosing);
			} else {
				theDbg.logInfo("BonyRenderContext returned null JFrame, so we have no window to close.");
			}
		} else {
			theDbg.logWarning("shutdown...() found null BonyRenderContext");
		}
	}
------
WorkaroundFuncsMustDie.java

	public static void setupRegularCameraLightAndViewport(WorkaroundAppStub was) {
		FlyByCamera fbc = was.getFlyByCamera();
        fbc.setDragToRotate(true);
		//fbc.setMoveSpeed(10f); //This is set in HumanoidRenderContext.initCameraAndLights()
		was.setPauseOnLostFocus(false);
		//ViewPort vp = was.getPrimaryAppViewPort(); // Background color now set in HumanoidRenderWorldMapper.setBackgroundColor
		//vp.setBackgroundColor(ColorRGBA.LightGray);
//    initKeys();

		// JME2-only so far, it seems
		// JoystickInput.setProvider( InputSystem.INPUT_SYSTEM_LWJGL );

	}
	public static Canvas makeAWTCanvas(SimpleApplication app) {
		AppSettings settings = app.getContext().getSettings();
		theLogger.info("making AWTCanvas in WorkaroundFuncsMustDie: Size is {}x{}", settings.getWidth(), settings.getHeight());
		return makeAWTCanvas(app, settings.getWidth(), settings.getHeight());
	}
	public static Canvas makeAWTCanvas(SimpleApplication app, int width, int height) {
	*/

/* In a silent applet, we might do:
 *         settings.setAudioRenderer(null);
		 // setLowPermissions has important effects on native libs and classpath resource loading.
        JmeSystem.setLowPermissions(true);
 */
/*
		// This causes JME ClasspathLocator to use:
		//							url = ClasspathLocator.class.getResource("/" + name);
        //			instead of		url = Thread.currentThread().getContextClassLoader().getResource(name);
		// JmeSystem.setLowPermissions(true);
		// Again, see new workaround in Activator (actually SETTING the contextClassLoader!!)

		JmeContext ctx = app.getContext();
		JmeCanvasContext cctx = (JmeCanvasContext) ctx;
		Canvas awtCanvas = cctx.getCanvas();
        awtCanvas.setSize(width, height);
		return awtCanvas;
	}

*/