/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity;
import org.jfugue.*;
/**
 *
 * @author winston
 */
public class JFugueTest {
    public static void main(String[] args)     {
        Player player = new Player();
        Pattern pattern = new Pattern("C D E F G A B");
        player.play(pattern);
        player.play("E5s A5s C6s B5s E5s B5s D6s C6i E6i G#5i E6i | A5s E5s A5s C6s B5s E5s B5s D6s C6i A5i Ri");
// "Frere Jacques"
Pattern pattern1 = new Pattern("C5q D5q E5q C5q");

// "Dormez-vous?"
Pattern pattern2 = new Pattern("E5q F5q G5h");

// "Sonnez les matines"
Pattern pattern3 = new Pattern("G5i A5i G5i F5i E5q C5q");

// "Ding ding dong"
Pattern pattern4 = new Pattern("C5q G4q C5h");

// Put all of the patters together to form the song
Pattern song = new Pattern();
song.add(pattern1, 2); // Adds 'pattern1' to 'song' twice
song.add(pattern2, 2); // Adds 'pattern2' to 'song' twice
song.add(pattern3, 2); // Adds 'pattern3' to 'song' twice
song.add(pattern4, 2); // Adds 'pattern4' to 'song' twice
player.play(song);
Pattern doubleMeasureRest = new Pattern("Rw Rw");

// Create the first voice
Pattern round1 = new Pattern("V0");
round1.add(song);

// Create the second voice
Pattern round2 = new Pattern("V1");
round2.add(doubleMeasureRest);
round2.add(song);

// Create the third voice
Pattern round3 = new Pattern("V2");
round3.add(doubleMeasureRest, 2);
round3.add(song);

// Put the voices together
Pattern roundSong = new Pattern();
roundSong.add(round1);
roundSong.add(round2);
roundSong.add(round3);

// Play the song!
player.play(roundSong);

 	Rhythm rhythm = new Rhythm();
// Bang out your drum beat
        rhythm.setLayer(1, "O..oO...O..oOO..");
rhythm.setLayer(2, "..*...*...*...*.");
rhythm.setLayer(3, "^^^^^^^^^^^^^^^^");
rhythm.setLayer(4, "...............!");
//Associate percussion notes with your beat
rhythm.addSubstitution('O', "[BASS_DRUM]i");
rhythm.addSubstitution('o', "Rs [BASS_DRUM]s");
rhythm.addSubstitution('*', "[ACOUSTIC_SNARE]i");
rhythm.addSubstitution('^', "[PEDAL_HI_HAT]s Rs");
rhythm.addSubstitution('!', "[CRASH_CYMBAL_1]s Rs");
rhythm.addSubstitution('.', "Ri");
//Play the rhythm!
Pattern rpat = rhythm.getPattern();
rpat.repeat(4);
player.play(rpat);
        System.exit(0); // If using Java 1.4 or lower
    }
}
