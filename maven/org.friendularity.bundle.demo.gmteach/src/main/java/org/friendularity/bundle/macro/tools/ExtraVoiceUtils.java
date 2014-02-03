/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.tools;

import java.util.List;

import org.jflux.impl.services.rk.lifecycle.utils.ManagedServiceFactory;
import org.mechio.api.motion.Robot;
import org.mechio.impl.speech.RemoteSpeechUtils;
import org.mechio.integration.motion_speech.VisemeMotionUtils;

/**
 *
 * @author matt
 */
public class ExtraVoiceUtils {
	public final static String SPEECH_SERVICE_ID_SUFFIX = "SpeechService";

	public static void startExtraVoices(ManagedServiceFactory fact, Robot.Id robotId, List<String> chans, String connectConfigId, String visemePath) {
		for (String chan : chans) {
			startExtraVoice(fact, robotId, chan, connectConfigId, visemePath);
		}
	}

	public static void startExtraVoice(ManagedServiceFactory fact, Robot.Id robotId, String chan, String connectConfigId, String visemePath) {
		String speechId = chan + SPEECH_SERVICE_ID_SUFFIX;
		RemoteSpeechUtils.connect(fact, speechId, chan, connectConfigId);
		VisemeMotionUtils.startVisemeFrameSourceGroup(fact, robotId, speechId, visemePath);
	}
}
