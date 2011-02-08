package rollmadness.formats;

public class TrackInfo {
    private final int hitPercentageToWin;
    private final String modelName;
    private int targetCount;
    private int destroyedTargetCount;
    private final String trackName;

    public TrackInfo(org.w3c.dom.Element node) {
	trackName = node.getElementsByTagName("name").item(0).getTextContent().trim();
	modelName = node.getElementsByTagName("model").item(0).getTextContent().trim();
	hitPercentageToWin = Integer.parseInt(node.getElementsByTagName("hitpercentagetowin").item(0).getTextContent().trim());
    }

    public TrackInfo(String trackName, String modelName, int tpc) {
	this.trackName = trackName;
	this.hitPercentageToWin = tpc;
	this.modelName = modelName;
    }

    public int getHitPercentageToWin() {
	return hitPercentageToWin;
    }

    public String getModelName() {
	return modelName;
    }

    public int getTargetCount() {
	return targetCount;
    }

    public void setTargetCount(int targetCount) {
	this.targetCount = targetCount;
    }

    public int getDestroyedTargetCount() {
	return destroyedTargetCount;
    }

    public void setDestroyedTargetCount(int destroyedTargetCount) {
	this.destroyedTargetCount = destroyedTargetCount;
    }

    public String getTrackName() {
	return trackName;
    }
}
