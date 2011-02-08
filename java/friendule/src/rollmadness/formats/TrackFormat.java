package rollmadness.formats;

public interface TrackFormat {
    String ENERGY_BONUS = "energy";
    String AMMO_BONUS = "ammo";
    
    //Makes the player go faster in the local direction of the sprinter
    String SPRINTER = "sprinter";

    String PLAYER_ID = "player";
    String WALL_ID = "wall";
    String TRACK_ID = "track";
    String SKYBOX_ID = "skybox";


    String CAPSULE_SCREEN_ID = "screen";

    String TARGET_ID = "target";

    /**
     * Any element that is named volume is considered a bounding volume
     */
    String VOLUME_ID = "volume";

    /**
     * Any element that is a volume and whose name contains this id is considered
     * the goal volume (eg. the space that the player has to reach to end the game)
     */
    String GOAL_VOLUME_ID = "goal";

    String WARP_IN = "warpin";

    String WARP_OUT = "warpout";
}
