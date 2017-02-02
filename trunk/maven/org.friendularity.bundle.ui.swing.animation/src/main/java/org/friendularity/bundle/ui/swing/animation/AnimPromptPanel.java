/*
 * Copyright 2014 the Friendularity Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * AnimPromptPanel.java
 *
 * Created on Jul 13, 2012, 4:36:46 AM
 */
package org.friendularity.bundle.ui.swing.animation;

import org.jflux.api.common.rk.config.VersionProperty;
import org.jflux.api.core.Listener;
import org.jflux.api.core.Source;
import org.jflux.api.core.config.ConfigProperty;
import org.jflux.api.core.config.DefaultConfigProperty;
import org.jflux.api.core.node.ProducerNode;
import org.jflux.api.core.util.DefaultSource;
import org.jflux.api.core.util.EmptyAdapter;
import org.jflux.api.messaging.rk.services.ServiceCommand;
import org.jflux.impl.messaging.JMSAvroUtils;
import org.jflux.impl.messaging.rk.ServiceCommandRecord;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.DefaultAnimationLibrary;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;

/**
 * @author Matthew Stevenson
 */
public class AnimPromptPanel extends javax.swing.JPanel implements Source<AnimationLibrary> {
	private static final Logger theLogger = LoggerFactory.getLogger(AnimPromptPanel.class);
	private AnimationLibrary myLibrary;
	private Source<RemoteAnimationPlayerClient> myPlayerSource;

	/**
	 * Creates new form AnimPromptPanel
	 */
	public AnimPromptPanel() {
		initComponents();
		ConfigProperty<String> destProp =
				new DefaultConfigProperty<>(String.class,
						"animPrompt; {create: always, node: {type: topic}}");
		pnlPromptConnect.setDestination(destProp.getSource(), destProp.getSetter());
		ConfigProperty<String> ipAddrProp =
				new DefaultConfigProperty<>(String.class,
						"127.0.0.1");
		pnlPromptConnect.setBrokerAddress(ipAddrProp.getSource(), ipAddrProp.getSetter(),
				new DefaultSource<>("5672"),
				new DefaultSource<>(ConnectionUtils.getUsername()),
				new DefaultSource<>(ConnectionUtils.getPassword()),
				new DefaultSource<>("client1"),
				new DefaultSource<>("test"));
		myLibrary = new DefaultAnimationLibrary("prompt");
	}


	public void setPlayerSource(Source<RemoteAnimationPlayerClient> playerSource) {
		myPlayerSource = playerSource;
	}

	/**
	 * This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {

		pnlPromptConnect = new org.friendularity.bundle.ui.swing.animation.MessagingConnectPanel();
		btnConnect = new javax.swing.JButton();
		btnStart = new javax.swing.JButton();

		btnConnect.setText("Connect");
		btnConnect.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnConnectActionPerformed(evt);
			}
		});

		btnStart.setText("start");
		btnStart.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				btnStartActionPerformed(evt);
			}
		});

		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
		this.setLayout(layout);
		layout.setHorizontalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
						.addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
								.addComponent(pnlPromptConnect, javax.swing.GroupLayout.DEFAULT_SIZE, 502, Short.MAX_VALUE)
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
								.addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
										.addComponent(btnStart, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
										.addComponent(btnConnect, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
								.addContainerGap())
		);
		layout.setVerticalGroup(
				layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
						.addGroup(layout.createSequentialGroup()
								.addComponent(btnConnect)
								.addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
								.addComponent(btnStart))
						.addComponent(pnlPromptConnect, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
		);
	}// </editor-fold>//GEN-END:initComponents

	private ProducerNode<ServiceCommand> myPromptProducer;

	private void btnConnectActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnConnectActionPerformed
		pnlPromptConnect.connect();
		Session session = pnlPromptConnect.getSession();
		Destination dest = pnlPromptConnect.getDestination();
		myPromptProducer = buildSpeechRecChain(session, dest);
		myPromptProducer.getNotifier().addListener(
				new Listener<ServiceCommand>() {
					@Override
					public void handleEvent(ServiceCommand input) {
						String animName = input.getCommand();
						if (animName == null) {
							return;
						}
						RemoteAnimationPlayerClient player = myPlayerSource.getValue();
						if (player == null) {
							theLogger.info("No player set, unable to play: " + animName);
							return;
						}
						theLogger.info("Received Anim Prompt: " + animName);
						VersionProperty p = new VersionProperty(animName, "1.0");
						Animation anim = myLibrary.getAnimation(p);
						if (anim == null) {
							animName = animName.toLowerCase();
							p = new VersionProperty(animName, "1.0");
							anim = myLibrary.getAnimation(p);
						}
						if (anim == null) {
							animName = animName.toUpperCase();
							p = new VersionProperty(animName, "1.0");
							anim = myLibrary.getAnimation(p);
						}
						if (anim == null) {
							theLogger.info("No anim found for: " + animName);
							restartDefs();
							return;
						}
						if (myPlayerSource == null) {
							theLogger.info("No player set, unable to play: " + animName);
							return;
						}
						theLogger.info("Playing animation: " + animName);
						if (anim.getChannels().size() > 10) {
							stop("FACE");
						}
						stop("ARMS");
						player.playAnimation(anim);
						waitRestart(anim.getLength() + 1500);
					}
				});
		myPromptProducer.start();

	}//GEN-LAST:event_btnConnectActionPerformed


	private void waitRestart(final long time) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					Thread.sleep(time);
				} catch (Exception ex) {
				}
				restartDefs();
			}
		}).start();
	}

	private void btnStartActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStartActionPerformed
		startDefs();
	}//GEN-LAST:event_btnStartActionPerformed


	private ProducerNode<ServiceCommand> buildSpeechRecChain(
			Session session, Destination dest) {
		try {
			return JMSAvroUtils.buildEventReceiverChain(
					ServiceCommandRecord.class,
					ServiceCommandRecord.SCHEMA$,
					new EmptyAdapter(),
					session, dest);
		} catch (JMSException ex) {
			theLogger.warn("Error connecting to Anim Prompt.", ex);
			return null;
		}
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JButton btnConnect;
	private javax.swing.JButton btnStart;
	private org.friendularity.bundle.ui.swing.animation.MessagingConnectPanel pnlPromptConnect;
	// End of variables declaration//GEN-END:variables

	@Override
	public AnimationLibrary getValue() {
		return myLibrary;
	}

	private List<String> defAnims = Arrays.asList("LEGS", "BLINK", "FACE", "ARMS");

	private void startDefs() {
		for (String s : defAnims) {
			loop(s);
		}
	}

	private void restartDefs() {
		RemoteAnimationPlayerClient p = myPlayerSource.getValue();
		if (myLibrary == null || p == null) {
			return;
		}
		for (VersionProperty prop : myLibrary.getAnimationVersions()) {
			if (!defAnims.contains(prop.getName())) {
				Animation a = myLibrary.getAnimation(prop);
				p.stopAnimation(a);
			}
		}
		startDefs();
	}

	private void loop(String anim) {
		RemoteAnimationPlayerClient p = myPlayerSource.getValue();
		if (myLibrary == null || p == null) {
			return;
		}
		Animation legs = myLibrary.getAnimation(new VersionProperty(anim, "1.0"));
		if (legs != null) {
			p.loopAnimation(legs);
		}
	}

	private void stop(String anim) {
		RemoteAnimationPlayerClient p = myPlayerSource.getValue();
		if (myLibrary == null || p == null) {
			return;
		}
		Animation legs = myLibrary.getAnimation(new VersionProperty(anim, "1.0"));
		if (legs != null) {
			p.stopAnimation(legs);
		}
	}
}
