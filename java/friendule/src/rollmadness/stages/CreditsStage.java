package rollmadness.stages;

import java.awt.Dimension;
import javax.swing.ImageIcon;
import jme3ui.event.action.UIAction;
import jme3ui.event.action.UIActionListener;
import jme3ui.layouts.UIGridLayout;
import jme3ui.widgets.UIButton;
import jme3ui.widgets.UIFrame;
import jme3ui.widgets.UILabel;
import jme3ui.widgets.UIPanel;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class CreditsStage extends GameStage {
    private final UIFrame FRAME;

    public CreditsStage(GameStageEnvironment env) {
	super(env, CreditsStage.class.getName());
	UIButton back = new UIButton("Main Menu");
	UIPanel panel = new UIPanel();
	panel.setLayout(new UIGridLayout(3, 1));
	panel.add(new UILabel("Credits"));
	panel.add(new UILabel(new ImageIcon(getClass().getResource("/rollmadness/textures/monkey.png"))));
	panel.add(back);
	UIFrame frame = new UIFrame();
	frame.setContents(panel);
	back.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		jumpTo(MainMenu.class.getName());
	    }
	});
	FRAME = frame;
    }

    @Override
    public void start() {
	Dimension screenSize = getGameStageEnvironment().getScreenSize();
	FRAME.resizeAndCenter(new Dimension(300, 300), screenSize);
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
