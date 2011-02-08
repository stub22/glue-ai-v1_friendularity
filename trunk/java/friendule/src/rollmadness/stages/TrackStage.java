package rollmadness.stages;

import com.jme3.asset.AssetManager;
import com.jme3.audio.AudioNode;
import com.jme3.audio.AudioRenderer;
import com.jme3.bounding.BoundingVolume;
import com.jme3.bullet.collision.shapes.CollisionShape;
import com.jme3.bullet.collision.shapes.SphereCollisionShape;
import com.jme3.bullet.nodes.PhysicsNode;
import com.jme3.bullet.util.CollisionShapeFactory;
import com.jme3.collision.CollisionResults;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Ray;
import com.jme3.math.Transform;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import com.jme3.renderer.queue.RenderQueue.ShadowMode;
import com.jme3.scene.Geometry;
import com.jme3.scene.Spatial;
import com.jme3.texture.Texture2D;
import com.jme3.ui.Picture;
import java.awt.Dimension;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import jme3clogic.Condition;
import jme3clogic.Reaction;
import jme3clogic.Trigger;
import jme3clogic.TriggerSystem;
import jme3clogic.basic.MethodOwner;
import jme3clogic.basic.Threshold;
import jme3clogic.basic.conditions.ElapsedTime;
import jme3clogic.basic.conditions.NumericGreaterThan;
import jme3clogic.basic.conditions.SoundIsPlaying;
import jme3clogic.basic.conditions.SpatialIsMoving;
import jme3clogic.basic.conditions.SpatialIsOnGround;
import jme3clogic.basic.conditions.SpatialOutOfBound;
import jme3clogic.basic.reactions.AttachSpatial;
import jme3clogic.basic.reactions.CallMethod;
import jme3clogic.basic.reactions.DecreaseNumericField;
import jme3clogic.basic.reactions.Delay;
import jme3clogic.basic.reactions.DetachSpatial;
import jme3clogic.basic.reactions.IncreaseNumericField;
import jme3clogic.basic.reactions.PlaySound;
import jme3clogic.basic.reactions.RemoveTrigger;
import jme3clogic.basic.reactions.StopSound;
import jme3dae.ColladaDocumentFactory;
import jme3dae.ColladaDocumentFactory.FXSettingsGenerator;
import jme3dae.FXEnhancerInfo;
import jme3dae.FXEnhancerInfo.IgnoreMeasuringUnit;
import rollmadness.formats.TrackFormat;
import rollmadness.gameproperties.PropertyKey;
import rollmadness.gamestage.GameStage;
import rollmadness.gamestage.GameStageEnvironment;
import jme3clogic.basic.conditions.OneTimeSwitch;
import jme3clogic.basic.conditions.Switch;
import jme3clogic.basic.reactions.SwitchCondition;
import rollmadness.formats.TrackInfo;
import rollmadness.logic.conditions.MousePressedNoRepeat;
import rollmadness.logic.triggers.AimTargetingTrigger;
import rollmadness.logic.triggers.PickUpAmmo;
import rollmadness.logic.triggers.PickUpEnergy;
import rollmadness.logic.triggers.PickUpSprinterTrigger;
import rollmadness.logic.triggers.PlayerControlTriggers;
import rollmadness.logic.triggers.WarpTrigger;
import rollmadness.logic.triggers.WinTrigger;
import rollmadness.particleengine.BasicParticle;
import rollmadness.particleengine.Particle;
import rollmadness.particleengine.ParticleBehavior;
import rollmadness.particleengine.ParticleGroup;
import rollmadness.particleengine.ParticleRing;
import rollmadness.particleengine.behaviors.LinearBulletBehavior;
import rollmadness.particleengine.behaviors.PopStarBehavior;
import rollmadness.sceneparser.SceneObject;
import rollmadness.sceneparser.SceneObjectGenerator;
import rollmadness.sceneparser.SceneObjectMap;
import rollmadness.util.DefaultSkyBox;
import rollmadness.util.Lights;

public class TrackStage extends GameStage implements TrackFormat, MethodOwner {

    private static final String POPUP_GAMEOVER_METHOD = "popupGameOver";
    private final LinkedList<Spatial> visualElements = new LinkedList<Spatial>();
    private final LinkedList<Spatial> physicsElements = new LinkedList<Spatial>();
    private final LinkedList<Trigger> triggerElements = new LinkedList<Trigger>();
    private PhysicsNode playerNode;
    private Transform initialPlayerTransform;
    private final Spatial PLAYER_CAPSULE;
    private float PLAYER_RADIUS = 2;

    /**
     * The current track as loaded by the asset manager plugin
     */
    private Spatial scene;
    private PlayerHud playerHud;
    private final AudioNode shotSound, engineSound, rocketSound, rollingSound, 
	    windSound, targetHit, pickupAmmoSound, gameoverSound, pickupEnergySound,
	    warpSound;
    private final TriggerSystem triggerSystem = new TriggerSystem();
    private boolean paused;
    private Vector3f playerPausedVelocity = new Vector3f();
    private BoundingVolume trackBounds;
    private BoundingVolume goalVolume;
    private final Picture GAME_OVER = new Picture("gameover");
    private final Picture STAGE_COMPLETED = new Picture("stagecompleted");
    private Vector3f START_PUSH = new Vector3f(0, 0, 10);
    private SceneObjectMap targetObjects;
    private final DefaultSkyBox SKYBOX;

    public TrackStage(GameStageEnvironment env) {
	super(env, "TrackStage");
	Dimension screenSize = getGameStageEnvironment().getScreenSize();
	
	Texture2D scTexture = (Texture2D) env.getAssetManager().loadTexture(
		"rollmadness/textures/stagecompleted.png");
	STAGE_COMPLETED.setTexture(env.getAssetManager(), scTexture, true);
	STAGE_COMPLETED.setWidth(scTexture.getImage().getWidth());
	STAGE_COMPLETED.setHeight(scTexture.getImage().getHeight());
	STAGE_COMPLETED.setLocalTranslation(
		screenSize.width / 2 - scTexture.getImage().getWidth() / 2,
		screenSize.height / 2 - scTexture.getImage().getHeight() / 2,
		0);
	
	GAME_OVER.setImage(env.getAssetManager(),
		"rollmadness/textures/gameover.png", true);
	GAME_OVER.setWidth(354);
	GAME_OVER.setHeight(354);
	GAME_OVER.setLocalTranslation(
		screenSize.width / 2 - 354 / 2,
		screenSize.height / 2 - 354 / 2,
		0);

	FXEnhancerInfo bulletParticleFx = new FXSettingsGenerator().
		setIgnoreMeasuringUnit(IgnoreMeasuringUnit.ON).
		setIgnoreLights(FXEnhancerInfo.IgnoreLights.OFF).
		setUseJME3Materials(FXEnhancerInfo.UseJME3Materials.ON).
		setNormalMapGeneration(FXEnhancerInfo.NormalMapGenerator.ON).
		get();
	ColladaDocumentFactory.setFXEnhance(bulletParticleFx);
	createParticles();
	FXEnhancerInfo trackFx = new FXSettingsGenerator().
		setIgnoreMeasuringUnit(IgnoreMeasuringUnit.ON).
		setIgnoreLights(FXEnhancerInfo.IgnoreLights.OFF).
		setUseJME3Materials(FXEnhancerInfo.UseJME3Materials.ON).
		setNormalMapGeneration(FXEnhancerInfo.NormalMapGenerator.OFF).
		get();
	ColladaDocumentFactory.setFXEnhance(trackFx);

	gameoverSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/gameover.wav");
	rocketSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/rocket.wav");
	shotSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/dshk_3shots.wav");
	rollingSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/rolling002.wav");
	windSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/wind.wav");
	engineSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/engine.wav");
	targetHit = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/targethit.wav");
	pickupAmmoSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/pickupammo.wav");
	pickupEnergySound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/energy.wav");
	warpSound = new AudioNode(env.getAssetManager(),
		"rollmadness/sounds/warp.wav");
	engineSound.setVolume(0.25f);
	engineSound.setDirectional(false);
	engineSound.setLooping(true);
	shotSound.setLooping(false);
	shotSound.setDirectional(false);
	windSound.setLooping(true);
	windSound.setDirectional(false);
	rollingSound.setDirectional(false);
	rollingSound.setLooping(true);

	new Lights().createDirectionalLight(
		getGameStageEnvironment().getRootNode(),
		Vector3f.UNIT_XYZ.negate(), ColorRGBA.LightGray);
	new Lights().createDirectionalLight(
		getGameStageEnvironment().getRootNode(),
		new Vector3f(1, 0, 1), ColorRGBA.White);

	PLAYER_CAPSULE = env.getAssetManager().loadModel(
		"rollmadness/models/playercapsule.dae");

	SKYBOX = new DefaultSkyBox(env.getCamera(), env.getAssetManager());
    }

    public void playTrack(TrackInfo trackInfo) {
	getGameStageEnvironment().setGlobalProperty(PropertyKey.LAST_TRACK_INFO, trackInfo);
	String trackName = trackInfo.getModelName();
	cleanupElements();

	scene = getGameStageEnvironment().getAssetManager().loadModel(trackName);
	scene.updateGeometricState();
	scene.updateModelBound();

	SceneObjectGenerator sog = new SceneObjectGenerator();
	sog.setIgnoredCharacters("#").setMatchIfStartsWith(true);
	SceneObjectMap objects = sog.generate(scene, PLAYER_ID, TRACK_ID, WALL_ID, SKYBOX_ID);
	SceneObject skybox = objects.getFirstOf(SKYBOX_ID);
	if(skybox != null) {
	    skybox.detach();
	    SKYBOX.set(skybox.getWrappedSpatial());
	    SKYBOX.attachTo(getGameStageEnvironment().getRootNode());
	}
	targetObjects = new SceneObjectGenerator().setMatchIfStartsWith(true).
		generate(scene, TARGET_ID);
	
	SceneObjectMap volumes = new SceneObjectGenerator().setIgnoreCase(true).
		setIgnoredCharacters("#").setMatchIfStartsWith(true).
		generate(scene, TrackFormat.VOLUME_ID);
	for (SceneObject e : volumes.getSceneObjects(TrackFormat.VOLUME_ID)) {
	    BoundingVolume vol = e.getWrappedSpatial().getWorldBound();
	    if (e.getWrappedSpatial().getName().toLowerCase().contains(
		    TrackFormat.GOAL_VOLUME_ID)) {
		goalVolume = vol;
	    }
	    e.detach();
	}

	objects.detachAll(PLAYER_ID, WALL_ID, TRACK_ID);
	for (SceneObject sceneObject : objects.getSceneObjects(WALL_ID)) {
	    Spatial wall = sceneObject.getWrappedSpatial();
	    if (wall instanceof Geometry) {
		CollisionShape shape = CollisionShapeFactory.createMeshShape(wall);
		PhysicsNode node = new PhysicsNode(wall, shape, 0);
		node.setRestitution(1f);
		attachPhysicalElement(node);
		sceneObject.getSpatialParent().attachChild(node);
	    }
	}
	for (SceneObject sceneObject : objects.getSceneObjects(TRACK_ID)) {
	    Spatial track = sceneObject.getWrappedSpatial();
	    if (track instanceof Geometry) {
		CollisionShape shape = CollisionShapeFactory.createMeshShape(track);
		PhysicsNode node = new PhysicsNode(track, shape, 0);
		attachPhysicalElement(node);
		sceneObject.getSpatialParent().attachChild(node);
	    }
	}
	objects.reattachAll(WALL_ID, TRACK_ID);

	attachVisualElement(scene);
	scene.updateGeometricState();
	scene.updateModelBound();
	trackBounds = scene.getWorldBound();

	SceneObject player = objects.getFirstOf(PLAYER_ID);
	if (player != null) {
	    initialPlayerTransform = player.getWrappedSpatial().getLocalTransform();
	    getGameStageEnvironment().getCamera().setFrame(
		    initialPlayerTransform.getTranslation().clone(),
		    initialPlayerTransform.getRotation().clone());

	    playerNode = new PhysicsNode(new SphereCollisionShape(PLAYER_RADIUS), 1);
	    playerNode.setLocalTranslation(initialPlayerTransform.getTranslation().clone());
	    attachVisualElement(playerNode);
	    attachPhysicalElement(playerNode);

	    final Camera cam = getGameStageEnvironment().getCamera();
	    PLAYER_CAPSULE.setLocalRotation(cam.getRotation());
	    PLAYER_CAPSULE.setLocalTranslation(cam.getLocation());
	    final Reaction updatePlayer = new Reaction() {

		@Override
		public void act(float timePerFrame) {
		    Vector3f pos = playerNode.getLocalTranslation();
		    cam.setLocation(pos);
		    PLAYER_CAPSULE.setLocalRotation(cam.getRotation());
		    PLAYER_CAPSULE.setLocalTranslation(cam.getLocation());
		}
	    };
	    PLAYER_CAPSULE.setCullHint(Spatial.CullHint.Never);
	    PLAYER_CAPSULE.setShadowMode(ShadowMode.Off);
	    attachVisualElement(PLAYER_CAPSULE);
	    attachTriggerElement(new Trigger(
		    Condition.ALWAYS,
		    updatePlayer));
	} else {
	    Logger.getLogger(getClass().getName()).log(Level.SEVERE,
		    "Cannot find player element in track " + trackName);
	}
    }

    public void attachTriggerElement(Trigger trigger) {
	triggerSystem.addTrigger(trigger);
	this.triggerElements.add(trigger);
    }

    protected void attachVisualElement(Spatial element) {
	getGameStageEnvironment().getRootNode().attachChild(element);
	this.visualElements.add(element);
    }

    protected void attachPhysicalElement(PhysicsNode node) {
	getGameStageEnvironment().getPhysicsSpace().add(node);
	this.physicsElements.add(node);
    }

    @Override
    public void start() {
	if (paused) {
	    paused = false;
	    triggerSystem.setEnabled(true);
	    playerNode.setLinearVelocity(playerPausedVelocity);
	    getGameStageEnvironment().setDefaultInputState(true);
	} else {
	    SKYBOX.attachTo(getGameStageEnvironment().getRootNode());
	    getGameStageEnvironment().getRootNode().addControl(triggerSystem);
	    playerHud = (PlayerHud) findStage("PlayerHud");
	    setHudTargetCount(playerHud, targetObjects);
	    if (playerHud != null && playerNode != null && initialPlayerTransform != null) {
		getGameStageEnvironment().getAudioRenderer().playSource(engineSound);
		getGameStageEnvironment().getRootNode().updateGeometricState();
		getGameStageEnvironment().getRootNode().updateModelBound();
		setupHudDisplay();
		createLogicTriggers();
		initialPlayerTransform.getRotation().multLocal(START_PUSH.set(0, 0, 10));
		playerNode.setLinearVelocity(START_PUSH);
	    } else {
		System.err.printf("Invalid State\nPlayerNode: %s\nInitialPlayerTransform: %s\n",
			playerNode, initialPlayerTransform);
	    }
	}
    }

    @Override
    public void pause() {
	paused = true;
	playerPausedVelocity.set(playerNode.getLinearVelocity());
	triggerSystem.setEnabled(false);
	playerNode.setLinearVelocity(new Vector3f(0, 0, 0));
	getGameStageEnvironment().setDefaultInputState(false);
    }

    @Override
    public void stop() {
	SKYBOX.detach();
	cleanupElements();
	playerHud.stop();
	stopAllSounds();
	getGameStageEnvironment().getRootNode().removeControl(triggerSystem);
	GAME_OVER.removeFromParent();
    }

    protected void cleanupElements() {
	SKYBOX.detach();
	STAGE_COMPLETED.removeFromParent();
	playerNode = null;
	initialPlayerTransform = null;
	while (!visualElements.isEmpty()) {
	    Spatial pop = visualElements.pop();
	    pop.removeFromParent();
	}
	while (!physicsElements.isEmpty()) {
	    Spatial pop = physicsElements.pop();
	    getGameStageEnvironment().getPhysicsSpace().remove(pop);
	}
	while (!triggerElements.isEmpty()) {
	    Trigger pop = triggerElements.pop();
	    pop.markForDeletion();
	}
    }

    protected void createLogicTriggers() {
	attachTriggerElement(new WarpTrigger(scene, playerNode, warpSound, getGameStageEnvironment().getAudioRenderer()));
	final List<Spatial> targetList = new LinkedList<Spatial>();
	for (SceneObject object : targetObjects.getSceneObjects(TrackFormat.TARGET_ID)) {
	    targetList.add(object.getWrappedSpatial());
	}
	AimTargetingTrigger att = new AimTargetingTrigger(getGameStageEnvironment().
		getCamera(), targetList, playerHud);
	attachTriggerElement(att);

	Condition rollsoundIsPlaying = new SoundIsPlaying(rollingSound);
	Condition rollsoundIsNotPlaying = rollsoundIsPlaying.negate();
	Condition playerIsOnGround = new SpatialIsOnGround(playerNode, scene,
		new Threshold(PLAYER_RADIUS + 1f));
	Condition playerIsFlying = playerIsOnGround.negate();
	Condition playerIsMoving = new SpatialIsMoving(playerNode,
		new Threshold(10f));
	Condition playerIsNotMoving = new SpatialIsMoving(playerNode,
		new Threshold(5f)).negate();
	
	Reaction startRollingSound = new PlaySound(rollingSound,
		getGameStageEnvironment().getAudioRenderer());
	Reaction stopRollingSound = new StopSound(rollingSound,
		getGameStageEnvironment().getAudioRenderer());
	Reaction startWindSound = new PlaySound(windSound,
		getGameStageEnvironment().getAudioRenderer());
	Reaction stopWindSound = new StopSound(windSound,
		getGameStageEnvironment().getAudioRenderer());
	final Reaction playTargetHitSound = new PlaySound(targetHit,
		getGameStageEnvironment().getAudioRenderer());

	Trigger playRollSound = new Trigger(
		playerIsMoving.and(rollsoundIsNotPlaying).and(playerIsOnGround),
		startRollingSound.and(stopWindSound));

	Trigger stopRollSound = new Trigger(
		playerIsNotMoving.or(rollsoundIsPlaying.and(playerIsFlying)),
		stopRollingSound);

	Trigger playWindSound = new Trigger(
		playerIsFlying, startWindSound);

	attachTriggerElement(playRollSound);
	attachTriggerElement(stopRollSound);
	attachTriggerElement(playWindSound);

	Condition fireWeapon =
		new MousePressedNoRepeat(getGameStageEnvironment().getInputManager(), 0);
	Condition hasAmmo = new NumericGreaterThan(playerHud, PlayerHud.AMMO, 0);
	Reaction playGunSound = new PlaySound(shotSound,
		getGameStageEnvironment().getAudioRenderer());
	Reaction reduceAmmo = new DecreaseNumericField(playerHud,
		PlayerHud.AMMO, 10);
	
	final Switch decreaseTargetCondition = new OneTimeSwitch(false);
	final Reaction decreaseTargetCount = new IncreaseNumericField(playerHud,
		PlayerHud.DESTROYED_TARGETS_COUNT, 1);
	attachTriggerElement(new Trigger(decreaseTargetCondition, decreaseTargetCount));
	Reaction checkTarget = new Reaction() {

	    final Ray ray = new Ray();
	    final CollisionResults res = new CollisionResults();
	    final Camera cam = getGameStageEnvironment().getCamera();
	    final Vector3f dir = new Vector3f(0, 0, 1);

	    @Override
	    public void act(float timePerFrame) {
		float bulletSpeed = getGameStageEnvironment().getGlobalProperty(
			PropertyKey.BULLET_SPEED, Number.class).floatValue();
		float bulletRange = getGameStageEnvironment().getGlobalProperty(
			PropertyKey.BULLET_RANGE, Number.class).floatValue();
		ParticleRing bullets = (ParticleRing) getGameStageEnvironment().
			getParticleEngine().getParticle("bullets");
		Particle bullet = bullets.next();
		bullet.setEnabled(false);
		ParticleBehavior bulletb = bullet.getBehavior();
		bulletb.setProperty(LinearBulletBehavior.ORIGIN, cam.getLocation().
			add(cam.getDirection().mult(1)));
		bulletb.setProperty(LinearBulletBehavior.DESTINATION,
			cam.getLocation().add(cam.getDirection().mult(bulletRange)));
		bulletb.setProperty(LinearBulletBehavior.SPEED, bulletSpeed);

		ray.setOrigin(cam.getLocation());
		ray.setDirection(cam.getDirection().normalize());
		ListIterator<Spatial> it = targetList.listIterator();
		while (it.hasNext()) {
		    Spatial next = it.next();
		    next.updateGeometricState();
		    next.updateModelBound();
		    final BoundingVolume bound = next.getWorldBound();
		    if (bound.intersects(ray)) {
			float distance = bound.getCenter().distance(cam.getLocation());
			if (distance < 100) {
			    float timeToHit = distance / bulletSpeed;
			    Trigger shot = new Trigger();
			    Condition elapsedTime = new ElapsedTime(timeToHit);
			    Reaction detachTarget = new DetachSpatial(next).and(
				    playTargetHitSound);
			    Reaction removeTrigger = new RemoveTrigger(shot);
			    Reaction popStars = new Reaction() {

				@Override
				public void act(float timePerFrame) {
				    System.out.println("popping stars at " + bound.getCenter());
				    ParticleRing starsRing = (ParticleRing) getGameStageEnvironment().getParticleEngine().getParticle("stars");
				    Particle star = starsRing.next();
				    star.getBehavior().setProperty(PopStarBehavior.KEY_ORIGIN, bound.getCenter());
				    star.setEnabled(true);
				}
			    };
			    shot.set(elapsedTime, popStars.and(detachTarget).and(removeTrigger).and(
				    new SwitchCondition(decreaseTargetCondition)));
			    getGameStageEnvironment().getTriggerSystem().addTrigger(
				    shot);
			    bulletb.setProperty(LinearBulletBehavior.DESTINATION,
				    bound.getCenter());
			    break;
			} else {
			    System.out.println("target too distant to be reached by current weapon");
			}
		    }
		}
		bullet.setEnabled(true);
	    }
	};
	Trigger playShot = new Trigger(hasAmmo.and(fireWeapon),
		playGunSound.and(reduceAmmo).and(checkTarget));
	attachTriggerElement(playShot);


	//update hud speed
	Condition elapsedTime = new ElapsedTime(2f);
	Reaction updateHudSpeed = new Reaction() {

	    @Override
	    public void act(float timePerFrame) {
		playerHud.set(PlayerHud.SPEED, playerNode.getLinearVelocity().length());
	    }
	};
	attachTriggerElement(new Trigger(elapsedTime, updateHudSpeed));

	//pickup energy balls, ammo balls
	SceneObjectMap pickups = new SceneObjectGenerator().setMatchIfStartsWith(true).
		generate(scene, TrackFormat.ENERGY_BONUS, TrackFormat.AMMO_BONUS,
		TrackFormat.SPRINTER);
	for (SceneObject sceneObject : pickups.getSceneObjects(TrackFormat.ENERGY_BONUS)) {
	    attachTriggerElement(new PickUpEnergy(playerNode,
		    sceneObject.getWrappedSpatial(), playerHud, pickupEnergySound));
	}
	for (SceneObject sceneObject : pickups.getSceneObjects(TrackFormat.AMMO_BONUS)) {
	    attachTriggerElement(new PickUpAmmo(playerNode,
		    sceneObject.getWrappedSpatial(), playerHud,
		    getGameStageEnvironment().getAudioRenderer(), pickupAmmoSound));
	}
	for (SceneObject sceneObject : pickups.getSceneObjects(TrackFormat.SPRINTER)) {
	    attachTriggerElement(new PickUpSprinterTrigger(playerNode,
		    sceneObject.getWrappedSpatial()));
	}

	//loss conditions: player out of track bounds
	Condition playerOutOfTrackBounds = new SpatialOutOfBound(trackBounds,
		playerNode).repeatAfter(new ElapsedTime(1f));
	Reaction popupGameOverStamp = new AttachSpatial(GAME_OVER,
		getGameStageEnvironment().getGuiNode());
	Reaction openGameOVerMenu = new CallMethod(this, POPUP_GAMEOVER_METHOD);
	Reaction waitFewSeconds = new Delay(2500L);
	Reaction playGameOverSound = new PlaySound(gameoverSound,
		getGameStageEnvironment().getAudioRenderer());
	Reaction gameOverReaction = 
		popupGameOverStamp.and(playGameOverSound).then(waitFewSeconds).
		then(openGameOVerMenu);

	//loss condition player stuck.
	Condition playerStuck = new Condition() {

	    private Vector3f lastpos;
	    private float timeline = 0;

	    @Override
	    public boolean holds(float timePerFrame) {
		if (lastpos == null) {
		    lastpos = new Vector3f(playerNode.getLocalTranslation());
		}
		timeline += timePerFrame;
		if (timeline > 10) {
		    timeline -= 10;
		    Vector3f currentPos = playerNode.getLocalTranslation();
		    float distance = currentPos.distance(lastpos);
		    lastpos.set(currentPos);
		    return distance < 5f;
		} else {
		    return false;
		}
	    }
	};
	Trigger gameOver = new Trigger(playerOutOfTrackBounds.or(playerStuck),
		gameOverReaction);

	attachTriggerElement(gameOver);
	attachTriggerElement(new WinTrigger(playerNode, goalVolume, this, gameOver));
	
	for (Trigger t : new PlayerControlTriggers(playerNode, playerHud,
		getGameStageEnvironment(), rocketSound).getTriggers()) {
	    attachTriggerElement(t);
	}
    }

    private void setupHudDisplay() {
	SceneObject screenObject = new SceneObjectGenerator().generate(
		PLAYER_CAPSULE, TrackFormat.CAPSULE_SCREEN_ID).
		getFirstOf(TrackFormat.CAPSULE_SCREEN_ID);
	Geometry geometry = screenObject.findGeometries().get(0);
	PlayerHud hud = (PlayerHud) findStage("PlayerHud");
	hud.setCapsuleDisplay(geometry);
	hud.start();
    }

    public void call(String method) {
	if (method.equals(POPUP_GAMEOVER_METHOD)) {
	    popupGameOver();
	} else if (method.equals("popupWinLabel")) {
	    popupWinLabel();
	} else if (method.equals("jumpToWinStage")) {
	    jumpToWinStage();
	} else {
	    Logger.getLogger(getClass().getName()).log(Level.WARNING, "Unrecognized method: " + method);
	}
    }

    private void popupGameOver() {
	stop();
	jumpTo(GameOver.class.getName());
    }

    private void stopAllSounds() {
	AudioRenderer ar = getGameStageEnvironment().getAudioRenderer();
	for (AudioNode audioNode : new AudioNode[]{shotSound, rollingSound,
		    rocketSound, windSound, engineSound, gameoverSound, pickupAmmoSound,
		    warpSound}) {
	    ar.stopSource(audioNode);
	}
    }

    private void createParticles() {
	AssetManager assetManager = getGameStageEnvironment().getAssetManager();
	Spatial bullet = getGameStageEnvironment().getAssetManager().
		loadModel("rollmadness/models/bullet.dae");
	Particle[] bullets = new Particle[10];
	for (int i = 0; i < bullets.length; i++) {
	    BasicParticle particle = new BasicParticle(bullet.clone());
	    particle.setBehavior(new LinearBulletBehavior());
	    bullets[i] = particle;
	}
	ParticleGroup[] stars = new ParticleGroup[10];
	for (int i = 0; i < stars.length; i++) {
	    Particle[] p = new Particle[5];
	    for (int j = 0; j < p.length; j++) {
		p[j] = new BasicParticle(assetManager, "rollmadness/textures/star.png").setBehavior(new PopStarBehavior(2f));
	    }
	    stars[i] = new ParticleGroup(p);
	}
	getGameStageEnvironment().getParticleEngine().addParticle("bullets", new ParticleRing(bullets));
	getGameStageEnvironment().getParticleEngine().addParticle("stars", new ParticleRing(stars));
    }

    private void setHudTargetCount(PlayerHud playerHud, SceneObjectMap objects) {
	playerHud.setTotalTargetCount(objects.getSceneObjects(TrackFormat.TARGET_ID).size());
    }

    private void popupWinLabel() {
	getGameStageEnvironment().getGuiNode().attachChild(STAGE_COMPLETED);
    }

    private void jumpToWinStage() {
	int intValue = playerHud.get(PlayerHud.DESTROYED_TARGETS_COUNT).intValue();
	TrackInfo trackInfo = getGameStageEnvironment().getGlobalProperty(
		PropertyKey.LAST_TRACK_INFO, TrackInfo.class);
	trackInfo.setDestroyedTargetCount(intValue);
	trackInfo.setTargetCount(targetObjects.getSceneObjects(TARGET_ID).size());
	jumpTo(WinStage.class.getName());
    }
}
