package org.friendularity.test.sensors.r50;

import org.jflux.api.core.Listener;
import org.robokind.api.sensor.DeviceBoolEvent;
import org.robokind.api.sensor.DeviceReadPeriodEvent;
import org.robokind.api.sensor.FilteredVector3Event;
import org.robokind.api.sensor.Vector3Event;
import org.robokind.api.sensor.gpio.RemoteGpioServiceClient;
import org.robokind.api.sensor.imu.RemoteAccelerometerServiceClient;
import org.robokind.api.sensor.imu.RemoteCompassServiceClient;
import org.robokind.api.sensor.imu.RemoteGyroscopeServiceClient;
import org.robokind.impl.sensor.AccelerometerConfigRecord;
import org.robokind.impl.sensor.CompassConfigRecord;
import org.robokind.impl.sensor.DeviceReadPeriodRecord;
import org.robokind.impl.sensor.GyroConfigRecord;
import org.robokind.impl.sensor.HeaderRecord;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) {
//        Sensors won't work on an avatar; you must use an actual robot.
//        Make sure the robot's sensor service is running!
//        Set the following to the robot's IP:
//        UserSettings.setSensorAddress("127.0.0.1");
//        UserSettings.setAccelerometerAddress("127.0.0.1");
//        UserSettings.setGyroscopeAddress("127.0.0.1");
//        UserSettings.setCompassAddress("127.0.0.1");
        UserSettings.setRobotId("myRobot");
        
        RemoteGpioServiceClient sensors = Robokind.connectSensors();
        DeviceReadPeriodEvent<HeaderRecord> readPeriod =
                new DeviceReadPeriodRecord();
        HeaderRecord header = new HeaderRecord();
        
        System.out.println("Adding pin direction.");
        
        sensors.setPinDirection(2, true);
        sensors.setPinDirection(4, true);
        
        System.out.println("Adding read period.");
        
        header.setFrameId(0);
        header.setSequenceId(0);
//        header.setTimestamp(TimeUtils.now());
        header.setTimestamp(0L);
        readPeriod.setHeader(header);
        readPeriod.setPeriod(100.0);
        
        sensors.setReadPeriod(readPeriod);
        
        System.out.println("Adding listener.");
        
        sensors.addListener(new TestGpioListener());
        
        System.out.println("Adding IMU.");
        
        RemoteAccelerometerServiceClient accel =
                Robokind.connectAccelerometer();
        RemoteGyroscopeServiceClient gyro = Robokind.connectGyroscope();
        RemoteCompassServiceClient compass = Robokind.connectCompass();
        
        readPeriod.setPeriod(1000.0);
        
        accel.setReadPeriod(readPeriod);
        gyro.setReadPeriod(readPeriod);
        compass.setReadPeriod(readPeriod);
        
        AccelerometerConfigRecord accelConfig = new AccelerometerConfigRecord();
        GyroConfigRecord gyroConfig = new GyroConfigRecord();
        CompassConfigRecord compassConfig = new CompassConfigRecord();
        
        accelConfig.setHeader(header);
        gyroConfig.setHeader(header);
        compassConfig.setHeader(header);
        
        accelConfig.setRegisterAddress(45);
        accelConfig.setRegisterValue(8);
        
        gyroConfig.setCtl1(15);
        gyroConfig.setCtl2(-1);
        gyroConfig.setCtl3(-1);
        gyroConfig.setCtl4(-1);
        gyroConfig.setCtl5(-1);
        
        compassConfig.setAverage(3);
        compassConfig.setBias(0);
        compassConfig.setGain(7);
        compassConfig.setRate(2);
        
        accel.sendConfig(accelConfig);
        gyro.sendConfig(gyroConfig);
        compass.sendConfig(compassConfig);
        
        accel.addListener(new TestAccelListener());
        gyro.addListener(new TestGyroListener());
        compass.addListener(new TestCompassListener());
    }
    
    private static class TestGpioListener implements Listener<DeviceBoolEvent> {
        @Override
        public void handleEvent(DeviceBoolEvent t) {
            System.out.println(t.getChannelId() + ": " +
                    (t.getBoolValue() ? "on" : "off"));
        }
    }
    
    private static class TestAccelListener
        implements Listener<FilteredVector3Event> {
        @Override
        public void handleEvent(FilteredVector3Event t) {
            Vector3Event v = t.getFilteredVector();
            Vector3Event r = t.getRawVector();
            System.out.println("Accelerometer (f): " + v.getX() + ", " +
                    v.getY() + ", " + v.getZ());
            System.out.println("Accelerometer (r): " + r.getX() + ", " +
                    r.getY() + ", " + r.getZ());
        }
    }
    
    private static class TestGyroListener
        implements Listener<FilteredVector3Event> {
        @Override
        public void handleEvent(FilteredVector3Event t) {
            Vector3Event v = t.getFilteredVector();
            Vector3Event r = t.getRawVector();
            System.out.println("Gyroscope (f): " + v.getX() + ", " +
                    v.getY() + ", " + v.getZ());
            System.out.println("Gyroscope (r): " + r.getX() + ", " +
                    r.getY() + ", " + r.getZ());
        }
    }
    
    private static class TestCompassListener
        implements Listener<FilteredVector3Event> {
        @Override
        public void handleEvent(FilteredVector3Event t) {
            Vector3Event v = t.getFilteredVector();
            Vector3Event r = t.getRawVector();
            System.out.println("Compass (f): " + v.getX() + ", " +
                    v.getY() + ", " + v.getZ());
            System.out.println("Compass (r): " + r.getX() + ", " +
                    r.getY() + ", " + r.getZ());
        }
    }
}
