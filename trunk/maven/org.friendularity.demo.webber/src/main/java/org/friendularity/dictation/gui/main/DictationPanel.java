/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * DictationPanel.java
 *
 * Created on Nov 2, 2009, 12:57:20 AM
 */

package org.friendularity.dictation.gui.main;

import org.friendularity.dictation.gui.advanced.AdvancedPanelFrame;
import org.friendularity.dictation.jmx.JMXInterface;
import java.awt.Color;
import java.awt.event.KeyEvent;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import org.friendularity.speech.CaptureThread;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.swing.text.DefaultCaret;
import org.friendularity.bind.cogbot.cogsim.DictationReciever;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;
import org.friendularity.webber.comm.Communicator;
import org.friendularity.webber.config.MeneConfig;
/**
 *
 * @author matt
 */
public class DictationPanel extends javax.swing.JPanel{
    private static Logger theLogger = Logger.getLogger(DictationPanel.class.getName());
    DictationImpl myImpl;
    private boolean isAdvancedPanelOpen = false;
    private AdvancedPanelFrame myAdvancedFrame;
    CogbotAvatar			myCSB;

    /** Creates new form DictationPanel */
    public DictationPanel() {		
        initComponents();

        
        txtMinSoundLength.getDocument().addDocumentListener(soundConfigValidator);
        txtVeryLongSoundLength.getDocument().addDocumentListener(soundConfigValidator);
        txtShortSilenceLength.getDocument().addDocumentListener(soundConfigValidator);
        txtLongSilenceLength.getDocument().addDocumentListener(soundConfigValidator);
        txtVolumeThreshold.getDocument().addDocumentListener(soundConfigValidator);
        txtBufferSize.getDocument().addDocumentListener(soundConfigValidator);

        myImpl = new DictationImpl(this);
        txtInput.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent e) {
                SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            txtConcatInput.setText(txtConcatInput.getText() + "" + txtInput.getText().toLowerCase());
                            txtInput.setText("");
                        }
                });
            }
            public void removeUpdate(DocumentEvent e) {}
            public void changedUpdate(DocumentEvent e) {}
        });

        txtMinSoundLength.setText(""+(long)Math.floor(myImpl.getSoundDetector().getCaptureThread().getMinSoundLength()));
        txtShortSilenceLength.setText(""+(long)Math.floor(myImpl.getSoundDetector().getCaptureThread().getShortSilenceLength()));
        txtLongSilenceLength.setText(""+(long)Math.floor(myImpl.getSoundDetector().getCaptureThread().getLongSilenceLength()));
        txtVolumeThreshold.setText(""+myImpl.getSoundDetector().getCaptureThread().getVolumeThreshold());
        txtBufferSize.setText(""+myImpl.getSoundDetector().getCaptureThread().getBufferSize());
        txtVeryLongSoundLength.setText(""+(long)Math.floor(myImpl.getSoundDetector().getCaptureThread().getVeryLongSoundLength()));
        chkSendOnSilence.setSelected(myImpl.getSoundDetector().getSendOnSilence());
        chkSendSoundCues.setSelected(myImpl.getSoundDetector().getSendSoundCues());

        myCSB =  CogbotService.getDefaultAvatar(MeneConfig.readPropertiesFile(Communicator.thePropsPath));
        myCSB.registerListener(new DictationReciever(){
            public void receiveNetworkText(String heard) {
                DictationImpl impl = getDictationImpl();
                if (impl!=null) impl.receiveNetworkText(heard);
            }
        });
        if (!myCSB.isCogSimEnabled) {
            theLogger.warning("isCogSimEnabled = " + myCSB.isCogSimEnabled + " so Cogbot may not know what the user is responding to");
        }
        if (!myCSB.isPolling) {
            theLogger.warning("isPolling = " + myCSB.isPolling + " so we will not recivie Sim conversation ");
        }
    }
	public DictationImpl getDictationImpl() {
		return myImpl;
	}
    DocumentListener soundConfigValidator = new DocumentListener()
    {
          public void insertUpdate(DocumentEvent e){
            validateAll();
          }
          public void removeUpdate(DocumentEvent e){
            validateAll();
          }
          public void changedUpdate(DocumentEvent e){
            validateAll();
          }

          private void validateAll() {
              boolean allValid=true;

              boolean minSoundLengthValid=validateMinSoundLength( txtMinSoundLength.getText() );
              allValid=allValid&&minSoundLengthValid;
              boolean veryLongSoundLengthValid=validateVeryLongSoundLength( txtVeryLongSoundLength.getText() );
              allValid=allValid&&veryLongSoundLengthValid;
              boolean soundLengthRangeValid=minSoundLengthValid && veryLongSoundLengthValid && validateSoundLengthRange(txtMinSoundLength.getText(), txtVeryLongSoundLength.getText());
              lblMinSoundLength.setForeground( (minSoundLengthValid && soundLengthRangeValid)?Color.black:Color.red  );
              txtMinSoundLength.setForeground( (minSoundLengthValid && soundLengthRangeValid)?Color.black:Color.red  );
              lblVeryLongSoundLength.setForeground( (veryLongSoundLengthValid && soundLengthRangeValid)?Color.black:Color.red  );
              txtVeryLongSoundLength.setForeground( (veryLongSoundLengthValid && soundLengthRangeValid)?Color.black:Color.red  );
              allValid=allValid && soundLengthRangeValid;

              boolean shortSilenceLengthValid=validateShortSilenceLength( txtShortSilenceLength.getText() );
              allValid=allValid&&shortSilenceLengthValid;
              boolean longSilenceLengthValid=validateLongSilenceLength( txtLongSilenceLength.getText() );
              allValid=allValid&&longSilenceLengthValid;
              boolean silenceLengthRangeValid=shortSilenceLengthValid && longSilenceLengthValid && validateSilenceLengthRange(txtShortSilenceLength.getText(), txtLongSilenceLength.getText());
              lblShortSilenceLength.setForeground( (shortSilenceLengthValid && silenceLengthRangeValid)?Color.black:Color.red  );
              txtShortSilenceLength.setForeground( (shortSilenceLengthValid && silenceLengthRangeValid)?Color.black:Color.red  );
              lblShortSilenceLength.setForeground( (longSilenceLengthValid && silenceLengthRangeValid)?Color.black:Color.red  );
              txtShortSilenceLength.setForeground( (longSilenceLengthValid && silenceLengthRangeValid)?Color.black:Color.red  );
              allValid=allValid && silenceLengthRangeValid;

              boolean volumeThresholdIsValid=validateVolumeThreshold( txtVolumeThreshold.getText() );
              lblVolumeThreshold.setForeground(volumeThresholdIsValid?Color.black:Color.red);
              txtVolumeThreshold.setForeground(volumeThresholdIsValid?Color.black:Color.red);
              allValid=allValid && volumeThresholdIsValid;

              boolean bufferSizeIsValid=validateBufferSize( txtBufferSize.getText() );
              lblBufferSize.setForeground(bufferSizeIsValid?Color.black:Color.red);
              txtBufferSize.setForeground(bufferSizeIsValid?Color.black:Color.red);
              allValid=allValid && bufferSizeIsValid;

              btnUpdateSoundConfigs.setEnabled(allValid);
          }

          private boolean validateMinSoundLength(String strVal){
            return validateLongWithRange(strVal,(long)Math.ceil(CaptureThread.MIN_SOUND_LENGTH_MIN_VALID_VALUE),(long)Math.floor(CaptureThread.MIN_SOUND_LENGTH_MAX_VALID_VALUE ));
          }

          private boolean validateVeryLongSoundLength(String strVal) {
              return validateLongWithRange(strVal,(long)Math.ceil(CaptureThread.VERY_LONG_SOUND_LENGTH_MIN_VALID_VALUE),(long)Math.floor(CaptureThread.VERY_LONG_SOUND_LENGTH_MAX_VALID_VALUE ));
          }

          private boolean validateSoundLengthRange(String minSoundLengthStr, String veryLongSoundLengthStr){
            return validateLongComparisonFirstIsLess(minSoundLengthStr, veryLongSoundLengthStr);
          }

          private boolean validateShortSilenceLength(String strVal){
            return validateLongWithRange(strVal,(long)Math.ceil(CaptureThread.SHORT_SILENCE_LENGTH_MIN_VALID_VALUE),(long)Math.floor(CaptureThread.SHORT_SILENCE_LENGTH_MAX_VALID_VALUE));
          }

          private boolean validateLongSilenceLength(String strVal){
            return validateLongWithRange(strVal,(long)Math.ceil(CaptureThread.LONG_SILENCE_LENGTH_MIN_VALID_VALUE),(long)Math.floor(CaptureThread.LONG_SILENCE_LENGTH_MAX_VALID_VALUE));
          }

          private boolean validateSilenceLengthRange(String shortSilenceStr, String longSilenceStr) {
              return validateLongComparisonFirstIsLess(shortSilenceStr, longSilenceStr);
          }       
          
          private boolean validateVolumeThreshold(String strVal){
            return validateDoubleWithRange(strVal,CaptureThread.VOLUME_THRESHOLD_MIN_VALID_VALUE,CaptureThread.VOLUME_THRESHOLD_MAX_VALID_VALUE );
          }
          
          private boolean validateBufferSize(String strVal){
            return validateIntWithRange(strVal,CaptureThread.BUFFER_SIZE_MIN_VALID_VALUE,CaptureThread.BUFFER_SIZE_MAX_VALID_VALUE);
          }

          private boolean validateDoubleWithRange(String strVal, double min, double max) {
            try {
              double value=Double.parseDouble( strVal );
              return (value>=min && value<=max);
            }
            catch(Exception ex) {
              return false;
            }
          }

          private boolean validateLongWithRange(String strVal, long min, long max) {
            try {
              long value=Long.parseLong( strVal );
              return (value>=min && value<=max);
            }
            catch(Exception ex) {
              return false;
            }
          }
         
          private boolean validateIntWithRange(String strVal, int min, int max) {
            try {
              int value=Integer.parseInt( strVal );
              return (value>=min && value<=max);
            }
            catch(Exception ex) {
              return false;
            }
          }

          private boolean validateLongComparisonFirstIsLess(String value1Str, String value2Str){
            try{
               long value1 = Long.parseLong(value1Str);
               long value2 = Long.parseLong(value2Str);
               return value1<value2;
            }
            catch(Exception ex){
                return false;
            }
          }
    };

    public void setAdvancedPanelOpen(boolean b){
        isAdvancedPanelOpen = b;
    }

    public DictationImpl getImpl(){
        return myImpl;
    }

    public VolumeMonitorPanel getVolumeMonitorPanel(){
        return volumeMonitorPanel1;
    }

    private void sendMeaning(String meaning){
        Map<String,Double> meanings = new HashMap();
        meanings.put(meaning, 1.0);
        JMXInterface.sendResponseMeaningToConvoid(meanings);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        txtInput = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        txtMatches = new javax.swing.JTextArea();
        jScrollPane2 = new javax.swing.JScrollPane();
        txtConcatInput = new javax.swing.JTextArea();
        txtSendMeaning = new javax.swing.JTextField();
        btnSendMeaning = new javax.swing.JButton();
        txtTextCueName = new javax.swing.JTextField();
        btnSendTextCue = new javax.swing.JButton();
        txtTextCueText = new javax.swing.JTextField();
        lblMinSoundLength = new javax.swing.JLabel();
        lblShortSilenceLength = new javax.swing.JLabel();
        lblLongSilenceLength = new javax.swing.JLabel();
        lblVolumeThreshold = new javax.swing.JLabel();
        lblBufferSize = new javax.swing.JLabel();
        btnUpdateSoundConfigs = new javax.swing.JButton();
        txtMinSoundLength = new javax.swing.JFormattedTextField();
        txtShortSilenceLength = new javax.swing.JFormattedTextField();
        txtLongSilenceLength = new javax.swing.JFormattedTextField();
        txtVolumeThreshold = new javax.swing.JFormattedTextField();
        txtBufferSize = new javax.swing.JFormattedTextField();
        lblVeryLongSoundLength = new javax.swing.JLabel();
        txtVeryLongSoundLength = new javax.swing.JTextField();
        volumeMonitorPanel1 = new org.friendularity.dictation.gui.main.VolumeMonitorPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        chkSendOnSilence = new javax.swing.JCheckBox();
        jLabel5 = new javax.swing.JLabel();
        btnAdvancedVocabOptions = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        chkSendSoundCues = new javax.swing.JCheckBox();
        tglConnect = new javax.swing.JToggleButton();
        txtSendWait = new javax.swing.JFormattedTextField();
        lblBufferSize1 = new javax.swing.JLabel();

        setName("Form"); // NOI18N

        org.jdesktop.application.ResourceMap resourceMap = org.jdesktop.application.Application.getInstance().getContext().getResourceMap(DictationPanel.class);
        txtInput.setText(resourceMap.getString("txtInput.text")); // NOI18N
        txtInput.setName("txtInput"); // NOI18N
        txtInput.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                txtInput_keyPressed(evt);
            }
        });

        jScrollPane1.setName("jScrollPane1"); // NOI18N

        txtMatches.setColumns(20);
        txtMatches.setEditable(false);
        txtMatches.setRows(5);
        txtMatches.setName("txtMatches"); // NOI18N
        jScrollPane1.setViewportView(txtMatches);
        DefaultCaret caret = (DefaultCaret) txtMatches.getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);

        jScrollPane2.setName("jScrollPane2"); // NOI18N

        txtConcatInput.setColumns(20);
        txtConcatInput.setEditable(false);
        txtConcatInput.setRows(4);
        txtConcatInput.setTabSize(4);
        txtConcatInput.setName("txtConcatInput"); // NOI18N
        jScrollPane2.setViewportView(txtConcatInput);

        txtSendMeaning.setText(resourceMap.getString("txtSendMeaning.text")); // NOI18N
        txtSendMeaning.setName("txtSendMeaning"); // NOI18N
        txtSendMeaning.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                txtSendMeaningKeyPressed(evt);
            }
        });

        btnSendMeaning.setText(resourceMap.getString("btnSendMeaning.text")); // NOI18N
        btnSendMeaning.setName("btnSendMeaning"); // NOI18N
        btnSendMeaning.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSendMeaningActionPerformed(evt);
            }
        });

        txtTextCueName.setText(resourceMap.getString("txtTextCueName.text")); // NOI18N
        txtTextCueName.setName("txtTextCueName"); // NOI18N

        btnSendTextCue.setText(resourceMap.getString("btnSendTextCue.text")); // NOI18N
        btnSendTextCue.setName("btnSendTextCue"); // NOI18N
        btnSendTextCue.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                btnSendTextCueMouseClicked(evt);
            }
        });
        btnSendTextCue.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSendTextCueActionPerformed(evt);
            }
        });

        txtTextCueText.setText(resourceMap.getString("txtTextCueText.text")); // NOI18N
        txtTextCueText.setName("txtTextCueText"); // NOI18N
        txtTextCueText.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                txtTextCueTextKeyPressed(evt);
            }
        });

        lblMinSoundLength.setText(resourceMap.getString("lblMinSoundLength.text")); // NOI18N
        lblMinSoundLength.setName("lblMinSoundLength"); // NOI18N

        lblShortSilenceLength.setText(resourceMap.getString("lblShortSilenceLength.text")); // NOI18N
        lblShortSilenceLength.setName("lblShortSilenceLength"); // NOI18N

        lblLongSilenceLength.setText(resourceMap.getString("lblLongSilenceLength.text")); // NOI18N
        lblLongSilenceLength.setName("lblLongSilenceLength"); // NOI18N

        lblVolumeThreshold.setText(resourceMap.getString("lblVolumeThreshold.text")); // NOI18N
        lblVolumeThreshold.setName("lblVolumeThreshold"); // NOI18N

        lblBufferSize.setText(resourceMap.getString("lblBufferSize.text")); // NOI18N
        lblBufferSize.setName("lblBufferSize"); // NOI18N

        btnUpdateSoundConfigs.setText(resourceMap.getString("btnUpdateSoundConfigs.text")); // NOI18N
        btnUpdateSoundConfigs.setName("btnUpdateSoundConfigs"); // NOI18N
        btnUpdateSoundConfigs.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnUpdateSoundConfigsActionPerformed(evt);
            }
        });

        txtMinSoundLength.setColumns(5);
        txtMinSoundLength.setText(resourceMap.getString("txtMinSoundLength.text")); // NOI18N
        txtMinSoundLength.setName("txtMinSoundLength"); // NOI18N

        txtShortSilenceLength.setColumns(5);
        txtShortSilenceLength.setName("txtShortSilenceLength"); // NOI18N

        txtLongSilenceLength.setColumns(5);
        txtLongSilenceLength.setName("txtLongSilenceLength"); // NOI18N

        txtVolumeThreshold.setColumns(5);
        txtVolumeThreshold.setName("txtVolumeThreshold"); // NOI18N

        txtBufferSize.setColumns(5);
        txtBufferSize.setName("txtBufferSize"); // NOI18N

        lblVeryLongSoundLength.setText(resourceMap.getString("lblVeryLongSoundLength.text")); // NOI18N
        lblVeryLongSoundLength.setName("lblVeryLongSoundLength"); // NOI18N

        txtVeryLongSoundLength.setColumns(5);
        txtVeryLongSoundLength.setName("txtVeryLongSoundLength"); // NOI18N

        volumeMonitorPanel1.setName("volumeMonitorPanel1"); // NOI18N

        jLabel1.setText(resourceMap.getString("jLabel1.text")); // NOI18N
        jLabel1.setName("jLabel1"); // NOI18N

        jLabel2.setText(resourceMap.getString("jLabel2.text")); // NOI18N
        jLabel2.setName("jLabel2"); // NOI18N

        jLabel3.setText(resourceMap.getString("jLabel3.text")); // NOI18N
        jLabel3.setName("jLabel3"); // NOI18N

        jLabel4.setText(resourceMap.getString("jLabel4.text")); // NOI18N
        jLabel4.setName("jLabel4"); // NOI18N

        chkSendOnSilence.setText(resourceMap.getString("chkSendOnSilence.text")); // NOI18N
        chkSendOnSilence.setName("chkSendOnSilence"); // NOI18N
        chkSendOnSilence.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkSendOnSilenceMouseClicked(evt);
            }
        });
        chkSendOnSilence.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkSendOnSilenceActionPerformed(evt);
            }
        });

        jLabel5.setText(resourceMap.getString("jLabel5.text")); // NOI18N
        jLabel5.setName("jLabel5"); // NOI18N

        btnAdvancedVocabOptions.setText(resourceMap.getString("btnAdvancedVocabOptions.text")); // NOI18N
        btnAdvancedVocabOptions.setName("btnAdvancedVocabOptions"); // NOI18N
        btnAdvancedVocabOptions.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                btnAdvancedVocabOptionsMouseClicked(evt);
            }
        });

        jLabel6.setText(resourceMap.getString("jLabel6.text")); // NOI18N
        jLabel6.setName("jLabel6"); // NOI18N

        chkSendSoundCues.setText(resourceMap.getString("chkSendSoundCues.text")); // NOI18N
        chkSendSoundCues.setName("chkSendSoundCues"); // NOI18N
        chkSendSoundCues.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkSendSoundCuesMouseClicked(evt);
            }
        });
        chkSendSoundCues.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkSendSoundCuesActionPerformed(evt);
            }
        });

        tglConnect.setText(resourceMap.getString("tglConnect.text")); // NOI18N
        tglConnect.setName("tglConnect"); // NOI18N
        tglConnect.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tglConnectActionPerformed(evt);
            }
        });

        txtSendWait.setColumns(5);
        txtSendWait.setName("txtSendWait"); // NOI18N

        lblBufferSize1.setText(resourceMap.getString("lblBufferSize1.text")); // NOI18N
        lblBufferSize1.setName("lblBufferSize1"); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(volumeMonitorPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 254, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(lblMinSoundLength)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtMinSoundLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(lblLongSilenceLength, javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(lblShortSilenceLength, javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(lblVeryLongSoundLength, javax.swing.GroupLayout.Alignment.TRAILING))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(txtVeryLongSoundLength, 0, 0, Short.MAX_VALUE)
                            .addComponent(txtLongSilenceLength)
                            .addComponent(txtShortSilenceLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(0, 0, 0)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel3)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 16, Short.MAX_VALUE)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(lblVolumeThreshold, javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(lblBufferSize, javax.swing.GroupLayout.Alignment.TRAILING))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(txtBufferSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(txtVolumeThreshold, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(lblBufferSize1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(txtSendWait, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnUpdateSoundConfigs, javax.swing.GroupLayout.DEFAULT_SIZE, 146, Short.MAX_VALUE)))
                .addGap(10, 10, 10))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btnAdvancedVocabOptions, javax.swing.GroupLayout.DEFAULT_SIZE, 598, Short.MAX_VALUE)
                .addContainerGap())
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addGap(10, 10, 10)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 598, Short.MAX_VALUE)
                    .addComponent(txtInput, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 598, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 488, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(tglConnect, javax.swing.GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel5)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(chkSendOnSilence, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel6)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(chkSendSoundCues, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(txtSendMeaning, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 500, Short.MAX_VALUE)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(txtTextCueName, javax.swing.GroupLayout.PREFERRED_SIZE, 99, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(txtTextCueText, javax.swing.GroupLayout.DEFAULT_SIZE, 395, Short.MAX_VALUE)))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(btnSendTextCue, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(btnSendMeaning, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(txtInput, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel5)
                            .addComponent(chkSendOnSilence))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(chkSendSoundCues)
                            .addComponent(jLabel6))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(tglConnect))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 245, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtSendMeaning, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnSendMeaning))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnSendTextCue)
                    .addComponent(txtTextCueName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(txtTextCueText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                    .addComponent(lblMinSoundLength)
                                    .addComponent(txtMinSoundLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabel1))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                    .addComponent(lblVeryLongSoundLength)
                                    .addComponent(txtVeryLongSoundLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabel2))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                    .addComponent(lblShortSilenceLength)
                                    .addComponent(txtShortSilenceLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jLabel3)
                                    .addComponent(lblBufferSize1)
                                    .addComponent(txtSendWait, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                    .addComponent(lblVolumeThreshold)
                                    .addComponent(txtVolumeThreshold, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                    .addComponent(lblBufferSize)
                                    .addComponent(txtBufferSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(lblLongSilenceLength)
                            .addComponent(txtLongSilenceLength, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel4)
                            .addComponent(btnUpdateSoundConfigs)))
                    .addComponent(volumeMonitorPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnAdvancedVocabOptions)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

	private void txtSendMeaningKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txtSendMeaningKeyPressed
		if(evt.getKeyCode() == KeyEvent.VK_ENTER){
            sendMeaning(txtSendMeaning.getText());
		}
	}//GEN-LAST:event_txtSendMeaningKeyPressed

	private void btnSendTextCueMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_btnSendTextCueMouseClicked
		String name = txtTextCueName.getText();
		String meaning  = txtTextCueText.getText().toUpperCase();
		JMXInterface.sendInformationToConvoid(name, meaning);
	}//GEN-LAST:event_btnSendTextCueMouseClicked

	private void txtTextCueTextKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txtTextCueTextKeyPressed
        if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
            String name = txtTextCueName.getText();
            String meaning = txtTextCueText.getText().toUpperCase();
            JMXInterface.sendInformationToConvoid(name, meaning);
        }
	}//GEN-LAST:event_txtTextCueTextKeyPressed

    private void btnUpdateSoundConfigsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnUpdateSoundConfigsActionPerformed
        double volumeThreshold = Double.parseDouble(txtVolumeThreshold.getText());
        long minSoundLength = Long.parseLong(txtMinSoundLength.getText());
        long veryLongSoundLength = Long.parseLong(txtVeryLongSoundLength.getText());
        long shortSilenceLength = Long.parseLong(txtShortSilenceLength.getText());
        long longSilenceLength = Long.parseLong(txtLongSilenceLength.getText());
        long sendWaitLength = Long.parseLong(txtSendWait.getText());
        int bufferSize = Integer.parseInt(txtBufferSize.getText());
        myImpl.setSoundConfigs(volumeThreshold, minSoundLength, veryLongSoundLength, 
                               shortSilenceLength, longSilenceLength, bufferSize,
                               sendWaitLength);
    }//GEN-LAST:event_btnUpdateSoundConfigsActionPerformed

    private void chkSendOnSilenceMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkSendOnSilenceMouseClicked
        
    }//GEN-LAST:event_chkSendOnSilenceMouseClicked

    private void btnAdvancedVocabOptionsMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_btnAdvancedVocabOptionsMouseClicked
        if (!isAdvancedPanelOpen) {
            myAdvancedFrame = new AdvancedPanelFrame(this);
            myAdvancedFrame.setVisible(true);
            isAdvancedPanelOpen = true;
        } else {
            myAdvancedFrame.requestFocus();
        }
    }//GEN-LAST:event_btnAdvancedVocabOptionsMouseClicked

    private void txtInput_keyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txtInput_keyPressed
        if(evt.getKeyCode() == KeyEvent.VK_ENTER){
            if(myImpl.updateInput())
                myImpl.SendMatches();
        }
        if(evt.getKeyChar() == KeyEvent.VK_BACK_SPACE){
            String txt = txtConcatInput.getText();
            if(!txt.isEmpty()){
                txtConcatInput.setText(txt.substring(0, txt.length()-1));
            }
        }
}//GEN-LAST:event_txtInput_keyPressed

    private void chkSendSoundCuesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkSendSoundCuesMouseClicked
        
    }//GEN-LAST:event_chkSendSoundCuesMouseClicked

    private void tglConnectActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_tglConnectActionPerformed
        if(tglConnect.isSelected()){
            JMXInterface.connect();
            tglConnect.setEnabled(false);
        }
    }//GEN-LAST:event_tglConnectActionPerformed

    private void chkSendOnSilenceActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkSendOnSilenceActionPerformed
        boolean which = chkSendOnSilence.isSelected();
        myImpl.getSoundDetector().setSendOnSilence(which);
    }//GEN-LAST:event_chkSendOnSilenceActionPerformed

    private void chkSendSoundCuesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkSendSoundCuesActionPerformed
        boolean which = chkSendSoundCues.isSelected();
        myImpl.getSoundDetector().setSendSoundCues(which);
    }//GEN-LAST:event_chkSendSoundCuesActionPerformed

	private void btnSendMeaningActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSendMeaningActionPerformed
		sendMeaning(txtSendMeaning.getText());
	}//GEN-LAST:event_btnSendMeaningActionPerformed

	private void btnSendTextCueActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSendTextCueActionPerformed
		// TODO add your handling code here:
	}//GEN-LAST:event_btnSendTextCueActionPerformed

    public void setSoundConfigUIValues(String minSoundLengthText, String veryLongSoundLengthText,
            String shortSilenceLengthText, String longSilenceLengthText, String volumeThesholdText,
            String bufferSizeText, String sendWait, boolean sendOnSilence) {
        txtMinSoundLength.setText(minSoundLengthText);
        txtVeryLongSoundLength.setText(veryLongSoundLengthText);
        txtShortSilenceLength.setText(shortSilenceLengthText);
        txtLongSilenceLength.setText(longSilenceLengthText);
        txtVolumeThreshold.setText(volumeThesholdText);
        txtBufferSize.setText(bufferSizeText);
        txtSendWait.setText(sendWait);
        chkSendOnSilence.setSelected(sendOnSilence);
    }



    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAdvancedVocabOptions;
    private javax.swing.JButton btnSendMeaning;
    private javax.swing.JButton btnSendTextCue;
    private javax.swing.JButton btnUpdateSoundConfigs;
    private javax.swing.JCheckBox chkSendOnSilence;
    private javax.swing.JCheckBox chkSendSoundCues;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel lblBufferSize;
    private javax.swing.JLabel lblBufferSize1;
    private javax.swing.JLabel lblLongSilenceLength;
    private javax.swing.JLabel lblMinSoundLength;
    private javax.swing.JLabel lblShortSilenceLength;
    private javax.swing.JLabel lblVeryLongSoundLength;
    private javax.swing.JLabel lblVolumeThreshold;
    private javax.swing.JToggleButton tglConnect;
    private javax.swing.JFormattedTextField txtBufferSize;
    protected javax.swing.JTextArea txtConcatInput;
    protected javax.swing.JTextField txtInput;
    private javax.swing.JFormattedTextField txtLongSilenceLength;
    protected javax.swing.JTextArea txtMatches;
    private javax.swing.JFormattedTextField txtMinSoundLength;
    private javax.swing.JTextField txtSendMeaning;
    private javax.swing.JFormattedTextField txtSendWait;
    private javax.swing.JFormattedTextField txtShortSilenceLength;
    private javax.swing.JTextField txtTextCueName;
    private javax.swing.JTextField txtTextCueText;
    private javax.swing.JTextField txtVeryLongSoundLength;
    private javax.swing.JFormattedTextField txtVolumeThreshold;
    private org.friendularity.dictation.gui.main.VolumeMonitorPanel volumeMonitorPanel1;
    // End of variables declaration//GEN-END:variables
}
