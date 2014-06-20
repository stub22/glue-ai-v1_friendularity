/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.speech;

/**
 *
 * @author humankind
 */
public class CaptureThread extends Thread{
    
    private SoundDetector   mySoundDetector;

    
    public static final int BUFFER_SIZE_MIN_VALID_VALUE=1;
    public static final int DEFAULT_BUFFER_SIZE=64;
    public static final int BUFFER_SIZE_MAX_VALID_VALUE=Integer.MAX_VALUE;
    private int myBufferSize=DEFAULT_BUFFER_SIZE;

    //the minumum length for a sound to be considered.
    public static final long MIN_SOUND_LENGTH_MIN_VALID_VALUE=1;
    public static final long DEFAULT_MIN_SOUND_LENGTH=124L;
    public static final long MIN_SOUND_LENGTH_MAX_VALID_VALUE=Integer.MAX_VALUE;
    private long myMinSoundLength  = DEFAULT_MIN_SOUND_LENGTH;

    //the length of a "very long" sound. This is longer than a significant sound,
    //and corresponds to the robot being interrupted. This would effectively
    //tell the robot to shut up.
    public static final long VERY_LONG_SOUND_LENGTH_MIN_VALID_VALUE=1;
    public static final long DEFAULT_VERY_LONG_SOUND_LENGTH=1200L;
    public static final long VERY_LONG_SOUND_LENGTH_MAX_VALID_VALUE=Integer.MAX_VALUE;
    private long myVeryLongSoundLength=DEFAULT_VERY_LONG_SOUND_LENGTH;
    
    //the length of a "short" silence. This would (very roughly) correspond to a
    //comma or period, but not the end of a full thought. we watch for these so
    //that the robot can nod or say "ok", "uh-huh...", "i see...", or similar to
    //show that he is listening, without providing a 'full' response to what is
    //being said

    public static final long SHORT_SILENCE_LENGTH_MIN_VALID_VALUE=1;
    public static final long DEFAULT_SHORT_SILENCE_LENGTH=100L;
    public static final long SHORT_SILENCE_LENGTH_MAX_VALID_VALUE=Integer.MAX_VALUE;
    private long myShortSilenceLength=DEFAULT_SHORT_SILENCE_LENGTH;

    //the length of a 'long' silence, which usually correspondes to the end of a
    //complete thought, which dictation grabber will then find a meaning for and
    //send to Character Engine
    public static final long LONG_SILENCE_LENGTH_MIN_VALID_VALUE=1;
    public static final long DEFAULT_LONG_SILENCE_LENGTH = 700L;
    public static final long LONG_SILENCE_LENGTH_MAX_VALID_VALUE=Integer.MAX_VALUE;
    private long myLongSilenceLength=DEFAULT_LONG_SILENCE_LENGTH;

    //minimum volume
    public static final double VOLUME_THRESHOLD_MIN_VALID_VALUE=1;
    public static final double DEFAULT_VOLUME_THRESHOLD=10.0;
    public static final double VOLUME_THRESHOLD_MAX_VALID_VALUE=40.0d;
    private double myVolumeThreshold=DEFAULT_VOLUME_THRESHOLD;

    private Long myFirstIterationTime=null;

    //timestamp of current iteration. we grab this at the beginning of each
    //iteration of the while loop, so we have a consistent value for "now"
    //within the context of this loop iteration
    private Long myLastIterationTime=null;

    //whether or not we have heard a sound with length >= myMinSoundLength
    //since the last one was sent. once true, we wait for a silence with length >=LONG_SILENCE_LENGTH
    private boolean significantSoundHeard=false;
    private Long mySoundBeginning=null;
    private Long myLastSound=null;

    //whether or not we have heard a sound with length >= myVeryLongSoundLength
    private boolean veryLongSoundHeard=false;

    private boolean shortSilenceSent=false;
    private Long mySilenceBeginning=null;
    private Long myLastSilence=null;
    private long myMaxSoundLength = 8000L;


    public CaptureThread(SoundDetector sd) {
        mySoundDetector = sd;        
    }

    @Override
    public void run() {
      try {
         byte tempBuffer[] = new byte[myBufferSize];
         while(true) {
           int cnt = mySoundDetector.getDataLine().read(tempBuffer,0,tempBuffer.length);
           if(cnt<=0)
            {continue;}
           double vol = determine8BitMonoSignedLevel(tempBuffer, true, cnt);
           determineState(vol);
         } //end while(true)
       } //end try
      catch (Throwable t){
		  t.printStackTrace();
	  }
    }

    private synchronized void determineState(double vol) {
        determineState(vol, System.currentTimeMillis() );
    }

    private synchronized void determineState(double vol, long timeStamp) {
        if(myFirstIterationTime==null)
        { myFirstIterationTime=timeStamp; }
        
        if(myLastIterationTime!=null && timeStamp<myLastIterationTime)
        { throw new IllegalArgumentException("Timestamp must be greater than or equal to last timestamp");  }
        myLastIterationTime=timeStamp;

        if (vol > myVolumeThreshold) {
            //HANDLE SOUND...
            if (mySoundBeginning == null){
                mySoundBeginning =myLastIterationTime;
            }
            myLastSound = myLastIterationTime;
            long soundLength = getSoundLength();
            if(soundLength > myMaxSoundLength){
                mySoundBeginning = null;
                mySilenceBeginning = null;
                significantSoundHeard = false;
                veryLongSoundHeard = false;
                mySoundDetector.longSilenceDetected();
                shortSilenceSent = false;
                return;
            }
            //System.out.println(" " + soundLength + "/" + myMinSoundLength);
            if (!significantSoundHeard && soundLength > myMinSoundLength) {
                significantSoundHeard = true;
                mySoundDetector.soundStartDetected();
            }
            if(!veryLongSoundHeard && soundLength > myVeryLongSoundLength){
                veryLongSoundHeard = true;
                mySoundDetector.veryLongSoundDetected();
            }
            mySilenceBeginning = null;
            shortSilenceSent = false;
        } else {
            //HANDLE SILENCE...
            if (mySilenceBeginning == null){
                mySilenceBeginning = myLastIterationTime;
            }
            myLastSilence = myLastIterationTime;
            long silenceLength = getSilenceLength();
            if (significantSoundHeard && silenceLength > myShortSilenceLength && !shortSilenceSent) {
                mySoundDetector.shortSilenceDetected();
                shortSilenceSent = true;               
            }
            if (silenceLength > myLongSilenceLength) {
                mySoundBeginning = null; //reset the mySoundBeginning timestamp any time a long silence is heard...
                //...but only send the grabbed text if the silence was preceded by a sound of significant length
                if (significantSoundHeard) {
                    significantSoundHeard = false;
                    if(veryLongSoundHeard){
                        veryLongSoundHeard = false;
                    }
                }
                mySoundDetector.longSilenceDetected();
                mySilenceBeginning = null;
            }
        } //end else
    }

    public double determine8BitMonoSignedLevel(byte[] buffer, boolean signed, int len) {
        long bufferSum = 0;
        int bufferIndex = 0;
        while (bufferIndex < len) {
            if (signed){
                bufferSum += (Math.abs(buffer[bufferIndex]));
            }else{
                bufferSum += (Math.abs((byte)(buffer[bufferIndex]-128)));
            }
            
            bufferIndex += 2;
        }
        double averageVal = ((double) bufferSum / (len / 2.0));
		if(averageVal <= 1.0)
                { return 0; }
		
		double simplePercentage =  ((averageVal / ((double) Math.abs(Byte.MIN_VALUE))) * 100);
		double level = (double) (20 * (Math.log10(simplePercentage / 100.0)));
		//System.out.println(averageVal + ", " + simplePercentage + ", " + level);
		return level + 48;
	}

    public int getBufferSize(){
        return myBufferSize;
    }

    public long getMinSoundLength(){
        return myMinSoundLength;
    }

    public long getVeryLongSoundLength(){
        return myVeryLongSoundLength;
    }

    public long getShortSilenceLength(){
        return myShortSilenceLength;
    }

    public long getLongSilenceLength(){
        return myLongSilenceLength;
    }

    public double getVolumeThreshold(){
        return myVolumeThreshold;
    }

    public Long getFirstIterationTime(){
        return myFirstIterationTime;
    }

    public Long getLastIterationTime() {
        return myLastIterationTime;
    }

    public Long getElapsedTime(){
        if(myLastIterationTime==null || myFirstIterationTime==null)
         { return null; }
        return myLastIterationTime-myFirstIterationTime;
    }

    public boolean getSignificantSoundHeard(){
        return significantSoundHeard;
    }

    public boolean getVeryLongSoundHeard(){
        return veryLongSoundHeard;
    }

    public Long getSoundBeginning(){
        return mySoundBeginning;
    }

    public Long getLastSound(){
        return myLastSound;
    }

    public Long getSoundLength() {
        if(mySoundBeginning==null || myLastSound==null) {
            return null;
        }
        return myLastSound-mySoundBeginning;
    }

    public boolean getShortSilenceSent(){
        return shortSilenceSent;
    }

    public Long getSilenceBeginning(){
        return mySilenceBeginning;
    }

    public Long getLastSilence(){
        return myLastSilence;
    }

    public Long getSilenceLength() {
        if(mySilenceBeginning==null || myLastSilence==null) {
            return null;
        }
        return myLastSilence-mySilenceBeginning;
    }

    /*friend*/ synchronized void setSoundConfigs(double silenceThreshold, long minSoundLength, long veryLongSoundLength, long shortSilenceLength, long longSilenceLength, int bufferSize )
    {
        resetState();
        myVolumeThreshold=silenceThreshold;
        myMinSoundLength=minSoundLength;
        myVeryLongSoundLength=veryLongSoundLength;
        myShortSilenceLength=shortSilenceLength;
        myLongSilenceLength=longSilenceLength;
        myBufferSize=bufferSize;
    }

    private void resetState() {
        myFirstIterationTime=null;

        myLastIterationTime=null;

        significantSoundHeard=false;
        mySoundBeginning=null;
        myLastSound=null;

        veryLongSoundHeard=false;

        shortSilenceSent=false;
        mySilenceBeginning=null;
        myLastSilence=null;
    }
}