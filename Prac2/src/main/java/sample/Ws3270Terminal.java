package sample;

import java.io.*;
import java.nio.charset.Charset;

public class Ws3270Terminal{
    private static final String ASCII = "ascii";
    private static final String EXIT = "exit";
    private static final String OK = "ok";
    private static final String EXECUTABLE = "ws3270.exe";
    private static final String CONNECT = "connect";
    private static final String STRING = "string";
    private static final String ENTER = "enter";
    private static final String CLEAR = "clear";

    protected Process process = null;
         protected BufferedReader reader = null;
         protected Writer writer= null;
         public Ws3270Terminal(){
             try {
                 process = Runtime.getRuntime().exec(EXECUTABLE);
             } catch (FileNotFoundException e) {
                 e.printStackTrace();
                 System.exit(-1);
             } catch (IOException e) {
                 e.printStackTrace();
                 System.exit(-1);
             }
             reader = new BufferedReader(new InputStreamReader(process.getInputStream(),
                     Charset.forName("ASCII")));
             writer = new OutputStreamWriter(process.getOutputStream(),
                     Charset.forName("ASCII"));
         }

    private String sendLine(String line) {
        System.out.println(line.toString());
        try {
            this.writer.write(line);
            this.writer.flush();
            Thread.sleep(500);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        //Descomentar si se quiere ver todos los pasos
        //if(!(line.equals(ASCII) || line.equals(EXIT))) sendAscii();
        return line.equals(EXIT)?"":readResult().toString();
    }

    private StringBuilder readResult() {
        StringBuilder result = new StringBuilder();
        try {
            while (!reader.ready());
            while (!result.toString().contains(OK)) {
                result.append((char) reader.read());
            }
            System.out.println(result.toString());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }

    public void sendConnect(String hostname){
        sendLine(CONNECT+" "+hostname);
    }
    public void sendString(String string){
        sendLine(STRING+" "+string);
    }
    public void sendEnter(){
         sendLine(ENTER);
    }
    public String sendAscii(){
        return sendLine(ASCII);
    }
    public void disconnect(){
            sendLine(EXIT);
    }
    public void sendClear() {sendLine(CLEAR);}
}