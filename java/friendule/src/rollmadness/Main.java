package rollmadness;

import rollmadness.stages.MainMenu;
import com.jme3.app.SimpleBulletApplication;
import com.jme3.asset.plugins.UrlLocator;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import jme3clogic.TriggerSystem;
import jme3dae.ColladaLoader;
import jme3ui.core.UISystem;
import jme3ui.theme.UIThemeManager;
import jme3ui.theme.orange.OrangeUITheme;
import rollmadness.formats.TrackInfoSequence;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStageEnvironment;
import rollmadness.particleengine.ParticleEngine;
import rollmadness.stages.CreditsStage;
import rollmadness.stages.GameOver;
import rollmadness.stages.GameStart;
import rollmadness.stages.PlayerHud;
import rollmadness.stages.Settings;
import rollmadness.stages.TrackStage;
import rollmadness.stages.WinStage;
import rollmadness.util.MouseCamera;

public class Main extends SimpleBulletApplication implements GameStageEnvironment {
    static {
	Logger.getLogger("com.jme3").setLevel(Level.SEVERE);
    }

    public static void main(String[] args) {
        Main main = new Main();
	main.start();
    }

    private final Map<Object, Object> properties = new HashMap<Object, Object>();
    private final TriggerSystem triggerSystem = new TriggerSystem();
    private ParticleEngine particleEngine;
    private MouseCamera mcam;
    
    @Override
    public void simpleInitApp() {
	UIThemeManager.setDefaultTheme(new OrangeUITheme());
	mcam = new MouseCamera(cam, inputManager);
	flyCam.setEnabled(false);
	statsView.removeFromParent();
	UISystem.initialize(this);
	try {
	    properties.put(PropertyKey.TRACK_INFO_SEQUENCE, 
		    new TrackInfoSequence(getClass().
		    getResource("/rollmadness/formats/tracklist.xml")));
	} catch(Exception ex) {
	    Logger.getLogger(getClass().getName()).log(Level.SEVERE, "", ex);
	    return;
	}
	particleEngine = new ParticleEngine(rootNode);
	ColladaLoader.addTextureBase("rollmadness/textures/");
	assetManager.registerLocator(getClass().getResource("/rollmadness/textures").toString(), UrlLocator.class.getName());
	assetManager.registerLoader(ColladaLoader.class.getName(), "dae");
	rootNode.addControl(triggerSystem);
	TrackStage trackStage = new TrackStage(this);
	GameOver gameOver = new GameOver(this);
	WinStage trackWin = new WinStage(this);
	trackStage.addChild(gameOver);
	trackStage.addChild(trackWin);
	MainMenu mainMenu = new MainMenu(this);
	mainMenu.addChild(new CreditsStage(this));
	mainMenu.addChild(new Settings(this));
	mainMenu.addChild(new GameStart(this));
	mainMenu.addChild(trackStage);
	mainMenu.addChild(new PlayerHud(this));
	mainMenu.jumpTo(CreditsStage.class.getName());
    }

    public ParticleEngine getParticleEngine() {
	return particleEngine;
    }

    public TriggerSystem getTriggerSystem() {
	return triggerSystem;
    }

    public Dimension getScreenSize() {
	return new Dimension(settings.getWidth(), settings.getHeight());
    }

    public void setDefaultInputState(boolean enabled) {
	mcam.setEnabled(enabled);
	//flyCam.setEnabled(enabled);
    }

    public <T> T getGlobalProperty(Object key, Class<T> type) {
	return type.cast(properties.get(key));
    }

    public void setGlobalProperty(Object key, Object value) {
	properties.put(key, value);
    }
}
