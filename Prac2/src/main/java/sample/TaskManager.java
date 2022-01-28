package sample;


import java.util.ArrayList;
import java.util.List;

public class TaskManager {
    private final String HOSTNAME = "155.210.152.51:3270";
    private final String USERNAME = "grupo_05";
    private final String PASSWORD = "secreto6";
    private final String FILENAME = "tareas.c";
    private final String ASSIGN_CODE = "1";
    private final String VIEW_CODE = "2";
    private final String GENERAL_CODE = "1";
    private final String SPECIFIC_CODE = "2";
    private final String EXIT_CODE = "3";
    private final String TASK = "TASK";
    private final String OFF = "off";


    protected Ws3270Terminal terminal;
    public TaskManager(){
       this.terminal = new Ws3270Terminal();
    }

    public void start(){
        //conectarse al mainframe
        terminal.sendConnect(HOSTNAME);
        terminal.sendEnter();
        login();
        terminal.sendEnter();
        //ejecutar "tareas.c"
        terminal.sendString(FILENAME);
        terminal.sendEnter();
        terminal.sendAscii();
    }

    private void login(){
        terminal.sendString(USERNAME);
        terminal.sendEnter();
        terminal.sendString(PASSWORD);
        terminal.sendEnter();
    }
    public void assignGeneralTask( String desc, String date){
        selectOption(ASSIGN_CODE,GENERAL_CODE);
        terminal.sendString(date);
        terminal.sendEnter();
        terminal.sendString(desc);
        terminal.sendEnter();
        goToMenu();
    }

    public void assignSpecificTask(String name, String desc, String date){
        selectOption(ASSIGN_CODE,SPECIFIC_CODE);
        terminal.sendString(date);
        terminal.sendEnter();
        terminal.sendString(name);
        terminal.sendEnter();
        terminal.sendString(desc);
        terminal.sendEnter();
        goToMenu();
    }

    public List<Task> viewGeneralTasks(){
        selectOption(VIEW_CODE,GENERAL_CODE);
        return viewTasks(GENERAL_CODE);
    }

    public List<Task> viewSpecificTasks(){
        selectOption(VIEW_CODE,SPECIFIC_CODE);
        return viewTasks(SPECIFIC_CODE);
    }

    public List<Task> viewTasks(String taskType){
        List<Task> list = new ArrayList<Task>();
        String allTasks = terminal.sendAscii();
        String[] taskArray = allTasks.split("data:");
        for (String t: taskArray) {
            String[] fields = t.split("\\s+");
            if(fields.length>6 && fields[1].equals(TASK))
                list.add(getTask(fields));
        }
        goToMenu();
        return list;
    }

    private Task getTask(String[] fields){
        String number = fields[2].replace(":","");
        String date = fields[4];
        String name = fields[5];
        String description = fields[6];
        return new Task(number, name, description, date);
    }

    private void goToMenu(){
        terminal.sendString(EXIT_CODE);
        terminal.sendEnter();
    }
    private void selectOption(String option, String taskType){
        terminal.sendClear();
        terminal.sendString(option);
        terminal.sendEnter();
        terminal.sendString(taskType);
        terminal.sendEnter();
    }
    public void exit(){
        terminal.sendString(EXIT_CODE);
        terminal.sendEnter();
        terminal.sendEnter();
        terminal.sendString(OFF);
        terminal.sendEnter();
        terminal.disconnect();
    }
}
