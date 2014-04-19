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
 * AnimationLibraryPanel.java
 *
 * Created on Jul 13, 2012, 1:22:37 AM
 */
package org.friendularity.bundle.ui.swing.animation;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JFileChooser;
import org.jflux.api.common.rk.config.VersionProperty;
import org.jflux.api.core.Source;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.AnimationLibraryLoader;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;
import org.mechio.api.animation.xml.AnimationFileReader;
import org.mechio.api.animation.xml.AnimationXML;

/**
 *
 * @author Matthew Stevenson
 */
public class AnimationLibraryPanel extends javax.swing.JPanel {
    private AnimationLibrary myPromptLibrary;
    private Source<RemoteAnimationPlayerClient> myPlayerSource;
    private List<String> myFilters;
    
    /** Creates new form AnimationLibraryPanel */
    public AnimationLibraryPanel() {
        initComponents();
    }
    
    public void setPlayerSource(Source<RemoteAnimationPlayerClient> playerSource){
        animationListPanel1.setSource(playerSource);
        myPlayerSource = playerSource;
    }
    public void setPromptLibrary(AnimationLibrary library){
        myPromptLibrary = library;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        txtAnimationFolderPath = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        btnAdd = new javax.swing.JButton();
        btnClear = new javax.swing.JButton();
        btnBrowse = new javax.swing.JButton();
        animationListPanel1 = new org.friendularity.bundle.ui.swing.animation.AnimationListPanel();

        txtAnimationFolderPath.setText("./resources/animations/");

        jLabel1.setText("Animation Folder:");

        btnAdd.setText("Add Folder");
        btnAdd.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAddActionPerformed(evt);
            }
        });

        btnClear.setText("Clear Animations");
        btnClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnClearActionPerformed(evt);
            }
        });

        btnBrowse.setText("Browse");
        btnBrowse.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnBrowseActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btnClear))
            .addGroup(layout.createSequentialGroup()
                .addComponent(txtAnimationFolderPath, javax.swing.GroupLayout.DEFAULT_SIZE, 229, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnBrowse))
            .addComponent(animationListPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 301, Short.MAX_VALUE)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel1)
                    .addComponent(btnAdd))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtAnimationFolderPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnBrowse))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnAdd)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(animationListPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 425, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnClear))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void btnBrowseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnBrowseActionPerformed
        try{
            File file = chooseFile();
            if(file == null){
                return;
            }
            txtAnimationFolderPath.setText(file.getAbsolutePath());
        }catch(Throwable t){}
    }//GEN-LAST:event_btnBrowseActionPerformed

    private File chooseFile(){
        JFileChooser chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        
        chooser.setFileFilter(
                new javax.swing.filechooser.FileFilter() {

                    @Override
                    public boolean accept(File f) {
                        return f != null && f.isDirectory();
                    }

                    @Override
                    public String getDescription() {
                        return "Animation Directory";
                    }
                });
        int ret = chooser.showOpenDialog(null);
        if(ret != JFileChooser.APPROVE_OPTION){
            return null;
        }
        File file = chooser.getSelectedFile();
        if(file != null && !file.isDirectory()){
            return null;
        }
        return file;
    }
    
    public void addAnimationFolder(String folderPath){
        AnimationLibrary lib;
        try{
            AnimationFileReader reader = AnimationXML.getRegisteredReader();
            if(reader == null){
                return;
            }
            lib = AnimationLibraryLoader.loadAnimationFolder("library", reader, folderPath, true);
            if(lib == null){
                return;
            }
            List<Animation> animList = new ArrayList<Animation>();
            for(VersionProperty prop : lib.getAnimationVersions()){
                Animation anim = lib.getAnimation(prop);
                if(anim != null){
                    if(!animList.contains(anim)){
                        animList.add(anim);
                    }
                    if(myPromptLibrary != null){
                        myPromptLibrary.add(anim);
                    }
                }
            }
            animationListPanel1.addAnimations(animList);
        }catch(Throwable t){
        }
    }
    
    private void btnAddActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAddActionPerformed
        String path = txtAnimationFolderPath.getText();
        addAnimationFolder(path);
    }//GEN-LAST:event_btnAddActionPerformed

    private void btnClearActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearActionPerformed
        animationListPanel1.clearAnimations();
        if(myPromptLibrary != null){
            myPromptLibrary.clear();
        }
        repaint();
    }//GEN-LAST:event_btnClearActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private org.friendularity.bundle.ui.swing.animation.AnimationListPanel animationListPanel1;
    private javax.swing.JButton btnAdd;
    private javax.swing.JButton btnBrowse;
    private javax.swing.JButton btnClear;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JTextField txtAnimationFolderPath;
    // End of variables declaration//GEN-END:variables
}
