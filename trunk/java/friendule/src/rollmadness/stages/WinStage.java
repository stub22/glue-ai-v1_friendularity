package rollmadness.stages;

import java.awt.Dimension;
import jme3ui.event.action.UIAction;
import jme3ui.event.action.UIActionListener;
import jme3ui.layouts.RelativeLayout;
import jme3ui.layouts.RelativeLayout.UICellData;
import jme3ui.widgets.UIButton;
import jme3ui.widgets.UIFrame;
import jme3ui.widgets.UILabel;
import jme3ui.widgets.UIPanel;
import rollmadness.formats.TrackInfo;
import rollmadness.formats.TrackInfoSequence;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;

public class WinStage extends GameStage {
    private final UIFrame FRAME;
    private final UILabel trackName, availableTargets, targetsHit, hitPercentage,
	    requiredPercentage, status;
    private final UIButton tryAgain, mainMenu, gotoNextTrack;

    public WinStage(GameStageEnvironment env) {
	super(env, WinStage.class.getName());

	UIPanel panel = new UIPanel();
	panel.setLayout(new RelativeLayout());
	double row = 0;
	double rowHeight = 0.14;
	panel.add(trackName = new UILabel("TestTrack 001", UILabel.Align.LEADING), new UICellData(0, row, 1, rowHeight));
	row += rowHeight;
	panel.add(new UILabel("Available Targets: ", UILabel.Align.LEADING), new UICellData(0, row, 0.8, rowHeight));
	panel.add(availableTargets = new UILabel("9", UILabel.Align.TRAILING), new UICellData(0.8, row, 0.2, rowHeight));
	row += rowHeight;
	panel.add(new UILabel("Targets Hit: ", UILabel.Align.LEADING), new UICellData(0, row, 0.8, rowHeight));
	panel.add(targetsHit = new UILabel("9", UILabel.Align.TRAILING), new UICellData(0.8, row, 0.2, rowHeight));
	row += rowHeight;
	panel.add(new UILabel("Hit Percentage: ", UILabel.Align.LEADING), new UICellData(0, row, 0.8, rowHeight));
	panel.add(hitPercentage = new UILabel("50%", UILabel.Align.TRAILING), new UICellData(0.8, row, 0.2, rowHeight));
	row += rowHeight;
	panel.add(new UILabel("Required Percentage: ", UILabel.Align.LEADING), new UICellData(0, row, 0.8, rowHeight));
	panel.add(requiredPercentage = new UILabel("50%", UILabel.Align.TRAILING), new UICellData(0.8, row, 0.2, rowHeight));
	row += rowHeight;
	panel.add(new UILabel("Status: ", UILabel.Align.LEADING), new UICellData(0, row, 0.8, rowHeight));
	panel.add(status = new UILabel("Success", UILabel.Align.TRAILING), new UICellData(0.8, row, 0.2, rowHeight));
	row += rowHeight;

	panel.add(tryAgain = new UIButton("Try Again!"), new UICellData(0, row, 0.33, rowHeight));
	panel.add(mainMenu = new UIButton("Main Menu"), new UICellData(0.33, row, 0.33, rowHeight));
	panel.add(gotoNextTrack = new UIButton("Next Track"), new UICellData(0.66, row, 0.33, rowHeight));

	tryAgain.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		tryAgainLastTrack();
	    }
	});
	mainMenu.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		jumpToMainMenu();
	    }
	});
	gotoNextTrack.addUIActionListener(new UIActionListener() {

	    public void onUIAction(UIAction e) {
		gotoNextTrack();
	    }
	});

	final UIFrame frame = new UIFrame();
	frame.setContents(panel);
	FRAME = frame;
    }

    private void jumpToMainMenu() {
	jumpTo(MainMenu.class.getName());
    }

    private void gotoNextTrack() {
	TrackInfo curr = getGameStageEnvironment().getGlobalProperty(
		PropertyKey.LAST_TRACK_INFO, TrackInfo.class);
	TrackInfoSequence seq = getGameStageEnvironment().getGlobalProperty(
		PropertyKey.TRACK_INFO_SEQUENCE, TrackInfoSequence.class);
	TrackInfo nextTrack = seq.next(curr);
	if(nextTrack == null) {
	    jumpTo(CreditsStage.class.getName());
	} else {
	    getGameStageEnvironment().setGlobalProperty(
		    PropertyKey.NEXT_TRACK_INFO, nextTrack);
	    jumpTo(GameStart.class.getName());
	}
    }

    private void tryAgainLastTrack() {
	TrackInfo last = getGameStageEnvironment().getGlobalProperty(
		PropertyKey.LAST_TRACK_INFO, TrackInfo.class);
	getGameStageEnvironment().setGlobalProperty(PropertyKey.NEXT_TRACK_INFO,
		last);
	jumpTo(GameStart.class.getName());
    }

    @Override
    public void start() {
	final GameStageEnvironment env = getGameStageEnvironment();
	env.getInputManager().setCursorVisible(true);
	TrackInfo lastInfo = env.getGlobalProperty(PropertyKey.LAST_TRACK_INFO,
		TrackInfo.class);
	setScreenText(lastInfo.getTrackName(), lastInfo.getTargetCount(),
		lastInfo.getDestroyedTargetCount(), lastInfo.getHitPercentageToWin());
	Dimension screenSize = env.getScreenSize();
	FRAME.resizeAndCenter(new Dimension(400, 400), screenSize);
	FRAME.setVisible(true);
    }

    @Override
    public void pause() {
    }

    @Override
    public void stop() {
	FRAME.setVisible(false);
	getGameStageEnvironment().getInputManager().setCursorVisible(false);
    }

    private void setScreenText(String trackName, int targetCount, int destroyedTargetCount, int tpc) {
	int pc = (int)((destroyedTargetCount / (float) targetCount) * 100f);
	boolean success = pc >= tpc;
	gotoNextTrack.setEnabled(success);
	availableTargets.setText(String.valueOf(targetCount));
	hitPercentage.setText(String.valueOf(pc));
	requiredPercentage.setText(String.valueOf(tpc));
	status.setText(success ? "Success!" : "Failure");
	this.trackName.setText(trackName);
	targetsHit.setText(String.valueOf(destroyedTargetCount));
    }
}
