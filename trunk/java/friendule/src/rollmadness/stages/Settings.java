package rollmadness.stages;

import java.awt.Dimension;
import jme3ui.event.action.UIAction;
import jme3ui.event.action.UIActionListener;
import jme3ui.layouts.UIGridLayout;
import jme3ui.widgets.UIButton;
import jme3ui.widgets.UIFrame;
import jme3ui.widgets.UIPanel;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class Settings extends GameStage {
    private final UIFrame FRAME;

    public Settings(GameStageEnvironment env) {
	super(env, Settings.class.getName());
	UIFrame frame = new UIFrame();
	UIPanel panel = new UIPanel();
	panel.setLayout(new UIGridLayout(4, 1));

	final UIButton audio = new UIButton("Audio");
	final UIButton video = new UIButton("Video");
	final UIButton controls = new UIButton("Controls");
	final UIButton back = new UIButton("Back");
	UIActionListener listener = new UIActionListener() {

	    public void onUIAction(UIAction e) {
		if(e.getSource() == audio) {
		    
		} else if(e.getSource() == video) {
		    
		} else if(e.getSource() == controls) {
		    
		} else if(e.getSource() == back) {
		    goBack();
		}
	    }
	};
	
	audio.addUIActionListener(listener);
	video.addUIActionListener(listener);
	controls.addUIActionListener(listener);
	back.addUIActionListener(listener);

	panel.add(audio);
	panel.add(video);
	panel.add(controls);
	panel.add(back);

	frame.setContents(panel);

	FRAME = frame;
    }

    @Override
    public void start() {
	Dimension screenSize = getGameStageEnvironment().getScreenSize();
	FRAME.resizeAndCenter(new Dimension(300, 400), screenSize);
	FRAME.setVisible(true);
	getGameStageEnvironment().getInputManager().setCursorVisible(true);
    }

    @Override
    public void pause() {
    }

    @Override
    public void stop() {
	FRAME.setVisible(false);
	getGameStageEnvironment().getInputManager().setCursorVisible(false);
    }

}
