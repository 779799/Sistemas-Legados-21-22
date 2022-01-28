package com.unizar.legados.prac3.back.wrapper;

import com.sun.jna.platform.DesktopWindow;
import com.sun.jna.platform.WindowUtils;
import com.unizar.legados.prac3.back.model.Program;
import net.sourceforge.tess4j.ITesseract;
import net.sourceforge.tess4j.Tesseract1;
import net.sourceforge.tess4j.TesseractException;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.awt.image.MultiResolutionImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Wrapper {
    private Robot robot;
    private static Process process;
    private static ITesseract ocr;
    private static Rectangle screen;
    private static final int WIDTH = 428;
    private static final int HEIGHT = 290;
    private static final String EXECUTABLE = "Database-MSDOS/DOSBox-0.74\\DOSBox.exe " +
            "Database-MSDOS/Database\\gwbasic.bat -noconsole";
    private static final String BOX = "DOSBox";
    private static final int
            ORDER_OPT = KeyEvent.VK_3,
            INFO_OPT = KeyEvent.VK_4,
            SEARCH_TAPE_OPT = KeyEvent.VK_6,
            SEARCH_NAME_OPT = KeyEvent.VK_7;
    private static boolean fin = false;
    private static final String MENU = "ORDENAR";
    private static final String TYPES = "(ARCADE|JUEGO DE MESA|UTILIDAD|S. DEPORTI[A-Z]|" +
            "CONVERSACIONAL|SIMULADOR|VIDEOAVENTURA|ESTRATEGIA|\\-+)";

    public Wrapper() {
        try {
            process = Runtime.getRuntime().exec(EXECUTABLE);
            startRobot();
            startOCR();
            Thread.sleep(3000);
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void startRobot() {
        try {
            robot = new Robot();
        } catch (AWTException e) {
            e.printStackTrace();
        }
    }

    private void startOCR() {
        ocr = new Tesseract1();
        //OCR congifuration
        ocr.setVariable("preserve_interword_spaces", "1");
        ocr.setVariable("user_defined_dpi", "130");
        ocr.setDatapath("./tessdata");
        ocr.setLanguage("spa");
        setScreen();
    }

    //Set screen coordinates
    private void setScreen() {
        screen = null;
        for (DesktopWindow desktopWindow : WindowUtils.getAllWindows(false)) {
            if (desktopWindow.getTitle().contains(BOX)) {
                Rectangle windowRec = desktopWindow.getLocAndSize();
                int x = adjustScreen(windowRec.x);
                int y = adjustScreen(windowRec.y);
                screen = new Rectangle(x, y, WIDTH, HEIGHT);
                break;
            }
        }
    }

    private int adjustScreen(int size) {
        return (int) Math.round(size * 0.6855);
    }


    private String doCapture() {
        GraphicsDevice graphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        int screenWidth = graphicsDevice.getDisplayMode().getWidth();
        int screenHeight = graphicsDevice.getDisplayMode().getHeight();
        setScreen();
        MultiResolutionImage capture = robot.createMultiResolutionScreenCapture(screen);
        String result = "";
        try {
            result = ocr.doOCR((BufferedImage) capture.getResolutionVariant(
                    screenWidth, screenHeight));
        } catch (TesseractException e) {
            e.printStackTrace();
        }
        return result;
    }

    private int readNumber(String info) {
        int num = 0;
        String[] lines = info.split("\n");
        for (String l : lines) {
            if (l.contains("CONTIENE")) {
                String[] numLine = l.split("\\s+");
                if (numLine.length > 1) {
                    num = Integer.parseInt(numLine[1]);
                }
            }
        }
        return num;
    }

    public int viewInfo() {
        pressKeyAndWait(INFO_OPT, 2000);
        String info = doCapture();
        enter();
        return readNumber(info);
    }

    public List<Program> searchByTape(String tape) {
        List<Program> list = null;
        if (new Program().isNumeric(tape)) {
            list = searchByNumericTape(tape);
        } else {
            list = searchByLetterTape(tape);
        }
        return list;
    }

    public List<Program> searchByNumericTape(String tape) {
        pressKey(SEARCH_TAPE_OPT);
        enter();
        robot.delay(1500);
        String info = doCapture();
        List<Program> list = new ArrayList<Program>();
        while (!info.contains(MENU)) {
            pressKeyAndWait(KeyEvent.VK_SPACE, 1500);
            list.addAll(parseLines(info, tape));
            info = doCapture();
        }
        enter();
        typeNo();
        return list;
    }

    private List<Program> parseLines(String info, String tape) {
        String[] lines = info.split("\n");
        List<Program> list = new ArrayList<>();
        for (String l : lines) {
            Program p = matchWithId(l);
            if (!p.isEmpty() && p.inTape(tape)) {
                list.add(p);
            }
        }
        return list;
    }

    public List<Program> searchByLetterTape(String tape) {
        //Order programs by tape
        pressKey(ORDER_OPT);
        typeLine("3");
        robot.delay(28000);
        enter();
        //Start where the first program of the specified tape
        pressKey(SEARCH_TAPE_OPT);
        typeLine(tape);
        robot.delay(1500);
        String info = doCapture();
        List<Program> list = new ArrayList<Program>();
        fin = false;
        while (!info.contains(MENU) && !fin) {
            pressKeyAndWait(KeyEvent.VK_SPACE, 1500);
            list.addAll(parseLinesAndCheck(info, tape));
            info = doCapture();
        }
        enter();
        typeNo();
        Collections.sort(list);
        return list;
    }

    private List<Program> parseLinesAndCheck(String info, String tape) {
        String[] lines = info.split("\n");
        List<Program> list = new ArrayList<>();
        for (String l : lines) {
            Program p = matchWithId(l);
            if (!p.isEmpty()) {
                if (p.noMoreInTape(tape)) {
                    fin = true;
                    break;
                }
                if (p.inTape(tape)) list.add(p);
            }
        }
        return list;
    }

    private Program matchWithId(String line) {
        String patternS = "[0-9]+ +(.+) +"+TYPES+" +([A-Z0-n\\-]+) +([0-9]+)";
        Pattern pattern = Pattern.compile(patternS);
        Matcher matcher = pattern.matcher(line);
        Program program = new Program();
        if (matcher.find()) {
            program = new Program(matcher.group(4), matcher.group(1), matcher.group(2), matcher.group(3));
        }
        return program;
    }

    private Program parse(String info) {
        String programLine = findProgram(info.split("\n"));
        return matchProgram(programLine);
    }

    private String findProgram(String[] lines) {
        String line = "";
        for (String l : lines) {
            if (l.contains("CINTA"))
                line = l;
        }
        return line;
    }

    private Program matchProgram(String programLine) {
        String patternS = "([0-9]+) +- +(.+) +"+TYPES+" +CINTA:([A-Z0-n\\-]+)";
        Pattern pattern = Pattern.compile(patternS);
        Matcher matcher = pattern.matcher(programLine);
        Program program = new Program();
        if (matcher.find()) {
            program = new Program(matcher.group(1), matcher.group(2), matcher.group(3), matcher.group(4));
        }
        return program;
    }

    public List<Program> searchByName(String name) {
        pressKey(SEARCH_NAME_OPT);
        typeNo();
        typeLine(name);
        robot.delay(2000);
        String info = doCapture();
        List<Program> list = new ArrayList<>();
        while (!info.contains("PULSA")) {
            Program program = parse(info);
            if (!program.isEmpty())
                list.add(program);
            typeNo();
            robot.delay(1000);
            info = doCapture();
        }
        enter();
        typeNo();
        return list;
    }


    private void pressKey(int key) {
        robot.keyPress(key);
        robot.delay(100);
        robot.keyRelease(key);
    }

    private void pressWithShift(int key) {
        robot.keyPress(KeyEvent.VK_SHIFT);
        robot.delay(100);
        pressKey(key);
        robot.keyRelease(KeyEvent.VK_SHIFT);
    }

    private void typeString(String line) {
        for (char c : line.toCharArray()) {
            int charCode = KeyEvent.getExtendedKeyCodeForChar(c);
            if (KeyEvent.CHAR_UNDEFINED != charCode) {
                if (!Character.isUpperCase(c))
                    pressKey(charCode);
                else
                    pressWithShift(charCode);
            }
        }
    }

    private void pressKeyAndWait(int key, int delay) {
        pressKey(key);
        robot.delay(delay);
    }

    private void typeNo() {
        typeLine("N");
    }

    private void enter() {
        pressKeyAndWait(KeyEvent.VK_ENTER, 700);
    }

    private void typeLine(String line) {
        typeString(line);
        enter();
    }


    public void finish() {
        process.destroy();
    }
}
