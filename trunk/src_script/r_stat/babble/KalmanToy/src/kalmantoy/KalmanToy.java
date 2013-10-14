/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package kalmantoy;
//import org.apache.commons.math3.util.*;
import java.io.*;
import java.io.FileOutputStream;
import org.apache.commons.math3.filter.*;
import org.apache.commons.math3.linear.*;
import org.apache.commons.math3.random.*;
import org.apache.commons.math3.analysis.function.*;

/**
 *
 * @author Mo
 */
public class KalmanToy {

    /**
     * @param args the command line arguments
     */
    // this code is to simulate the closed-loop / semi-supervised learning of a robot
    public static void main(String[] args) throws FileNotFoundException {
        System.out.println("BEGIN The algorithm...");
        
          
        
 //       double constantvalue = 1d;
        double measurementNoise = 0.4;
        double processNoise = 0.5;
        double input = 0.001;

// A = 2x2 matrix
        RealMatrix A = new Array2DRowRealMatrix(new double[][]{{0.7, -0.4}, {0, -0.5}});
        double[] x1 = new double[2];
// B = 2x1 matrix
        RealMatrix B = new Array2DRowRealMatrix(new double[][]{{1d}, {0d}});//null;
// H = 1x2 matrix
        RealMatrix H = new Array2DRowRealMatrix(new double[][]{{1.5d, 0d}});//
// x = 1X2 vector
        RealVector x = new ArrayRealVector(new double[]{0, 1});
// Q = 2x2 matrix
        RealMatrix Q = new Array2DRowRealMatrix(new double[][]{{processNoise, 0}, {0, 0.4}});
// P = 2x2 matrix
        RealMatrix P0 = new Array2DRowRealMatrix(new double[][]{{100, 0}, {0, 0.1}});
// R = [ 0.1 ]
        RealMatrix R = new Array2DRowRealMatrix(new double[][]{{measurementNoise}});

        ProcessModel pm = new DefaultProcessModel(A, B, Q, x, P0);
        MeasurementModel mm = new DefaultMeasurementModel(H, R);
        KalmanFilter filter = new KalmanFilter(pm, mm);

// process and measurement noise vectors
        RealVector pNoise = new ArrayRealVector(new double[2]);
        RealVector mNoise = new ArrayRealVector(new double[1]);
        RealVector u = new ArrayRealVector(new double[]{input});

        RealVector z = new ArrayRealVector(new double[600]);

        RandomGenerator rand = new JDKRandomGenerator();
        RealVector s = new ArrayRealVector(new double[600]);
        double[] erroriter= new double[10];
        Pow po= new Pow();

        // simulate the process
        double[] realstate= new double[600];
        System.out.println("Start Simulation...");
        for (int i = 0; i < 600; i++) {
            

            double tmp = rand.nextGaussian();
            pNoise.setEntry(0, processNoise * tmp);
            pNoise.setEntry(1, 1 * rand.nextGaussian());

// x = A * x + B*u + p_noise
            x = A.operate(x).add(B.operate(u.mapMultiply(1))).add(pNoise);
            realstate[i]=x.getEntry(0);
            x =x.add(pNoise);
//System.out.println("after adding pNoise");
// simulate the measurement
            mNoise.setEntry(0, measurementNoise * rand.nextGaussian());

//System.out.println("after 2nd setEntry");
// z = H * x + m_noise
            z.setSubVector(i, H.operate(x).add(mNoise));
            s.setEntry(i, x.getEntry(0));
        }
        System.out.println("Use Kalman filter as predictor...");
 
        RealMatrix R1 = new Array2DRowRealMatrix(new double[2][2]);
        RealMatrix R0 = new Array2DRowRealMatrix(new double[2][2]);
        RealMatrix Rxy = new Array2DRowRealMatrix(new double[2][1]);
        filter.predict();
        filter.correct(z.getSubVector(0, 1));
        // initialized with some prior knowledge
        
       File file = new File("test.txt");  
        FileOutputStream fis = new FileOutputStream(file);  
        PrintStream out = new PrintStream(fis);
        System.setOut(out); 
        x1 = filter.getStateEstimation();
            for (int i = 1; i < 600; i++) {
                filter.predict();
                //when observation Zk is available
                filter.correct(z.getSubVector(i, 1));
                x1 = filter.getStateEstimation();
                //plot the real value and estimated value
                System.out.println(realstate[i]+" "+x1[0]);             
            }      
    }
}