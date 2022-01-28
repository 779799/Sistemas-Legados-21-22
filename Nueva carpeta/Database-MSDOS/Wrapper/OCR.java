package Wrapper;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
public class OCR {
    private static Robot robot;
    private static Process process;
    private static int _x1;
    private static int _x2;
    private static int _y1;
    private static int _y2;

    public OCR() {
        String EXECUTABLE = "database.bat";
        String file = new File(".").getAbsolutePath();
        System.out.println(file);
        try {
            process = Runtime.getRuntime().exec(EXECUTABLE);
            robot = new Robot();
            Thread.sleep(2000);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(-1);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        } catch (AWTException e){
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        Frame[] windows = Frame.getFrames();
        System.out.println("WINDOWS---"+windows.length);
        for (Frame window : windows)
            System.out.println(window.getName() + ": " + window.getClass());

        //Rectangle rect = null;
        /*java.awt.Window
        for (DesktopWindow desktopWindow : WindowUtils.getAllWindows(true)) {
            if (desktopWindow.getTitle().contains("IDEA")) {
                rect = desktopWindow.getLocAndSize();
            }
        }
        WindowUtils.getAllWindows(true).forEach(desktopWindow -> {
            if (desktopWindow.getTitle().contains("IDEA")) {
                rect.setRect(desktopWindow.getLocAndSize());
            }
        });*/
    }

    private void pressKey(int key){
        robot.keyPress(key);
        robot.keyRelease(key);
    }

    private void pressKeyDelay(int key, int delay){
        robot.keyPress(key);
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        robot.keyRelease(key);
    }

    public int getNumRegister(){
        pressKey(KeyEvent.VK_4);
        //robot.createScreenCapture(new Rectangle(_x1, _y1, _x2 - _x1, _y2 - _y1));
        return 0;
    }
}