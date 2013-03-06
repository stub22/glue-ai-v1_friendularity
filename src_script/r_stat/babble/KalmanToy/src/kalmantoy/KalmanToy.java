/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package kalmantoy;
//import org.apache.commons.math3.util.*;
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
    public static void main(String[] args) {
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

// the targeted observation S

// iterate 60 time steps
        // simulate the process
        System.out.println("Start Simulation...");
        for (int i = 0; i < 600; i++) {
            

            double tmp = rand.nextGaussian();
            pNoise.setEntry(0, processNoise * tmp);
            pNoise.setEntry(1, 1 * rand.nextGaussian());

// x = A * x + B*u + p_noise
            x = A.operate(x).add(B.operate(u.mapMultiply(1))).add(pNoise);

//System.out.println("after adding pNoise");
// simulate the measurement
            mNoise.setEntry(0, measurementNoise * rand.nextGaussian());

//System.out.println("after 2nd setEntry");
// z = H * x + m_noise
            z.setSubVector(i, H.operate(x).add(mNoise));
            s.setEntry(i, x.getEntry(0));
        }
        System.out.println("Start Iterative EM algorithm...");
        for (int iter = 0; iter < 4; iter++) {
// E-step as kalman filter
        RealMatrix R1 = new Array2DRowRealMatrix(new double[2][2]);
        RealMatrix R0 = new Array2DRowRealMatrix(new double[2][2]);
        RealMatrix Rxy = new Array2DRowRealMatrix(new double[2][1]);
        filter.predict();
        filter.correct(z.getSubVector(0, 1));
        x1 = filter.getStateEstimation();
            for (int i = 1; i < 600; i++) {
                filter.predict();
                filter.correct(z.getSubVector(i, 1));

// compute the Rxx(0) and Rxx(1)
                RealVector tmpx1 = new ArrayRealVector(filter.getStateEstimation());
                RealMatrix tmpR1 = tmpx1.outerProduct(new ArrayRealVector(x1));
                R1 = R1.add(tmpR1);
                RealMatrix tmpR0 = tmpx1.outerProduct(tmpx1);
                R0 = R0.add(tmpR0);

// compute the Rxy(0)
                RealMatrix tmpRxy1 = tmpx1.outerProduct(z.getSubVector(i, 1));
                Rxy = Rxy.add(tmpRxy1);
                x1 = filter.getStateEstimation();

                //for(int ii=0; ii<filter.getStateEstimation().length ;ii++ )
                  
                //error of underlying state
                erroriter[iter] += po.value(filter.getStateEstimation()[0]-s.getEntry(i),2);
                
            }
            System.out.println(erroriter[iter]/600);
// M-step: update A and Q with AR model
// A= R(1)/R(0), Q= E(X(t)*e(t))= R(0)+a*R(1)- (B*u)'*(B*u)
            RealMatrix R0Inverse = new LUDecomposition(R0).getSolver().getInverse();
            A = R1.multiply(R0Inverse);
            Q = R0.add(A.multiply(R1)).subtract(B.multiply(B.transpose()).scalarMultiply(input * input));
//System.out.println(A.getColumn(0)[1]); 
// M-step: update H and R with regression method
// H = inv(Rxx)* Rxy, R= Rxy(0)-h*Rxx(0)
            H = R0Inverse.multiply(Rxy);
            H = H.transpose();
            //R=...
            
            //System.out.println(H.getColumn(0)[1]);
//Update the parameter
        pm = new DefaultProcessModel(A, B, Q, x, P0);
        mm = new DefaultMeasurementModel(H, R);
        filter = new KalmanFilter(pm, mm);
        }
    }//end of the EM algorithm
// choose the input u in order to minimize E[(s-z)^2]
}