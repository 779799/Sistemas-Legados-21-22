import sample.Task;
import sample.TaskManager;
import sample.Ws3270Terminal;

import java.util.List;

public class Main {
    public static void main(String[] args)
    {
        TaskManager manager = new TaskManager();
        manager.start();
        manager.assignGeneralTask("descrip","0101");
        manager.assignSpecificTask("name","desc","0204");
        List<Task> list = manager.viewGeneralTasks();
        for (Task t: list) {
            System.out.println(t.toString());
        }
        list = manager.viewSpecificTasks();
        for (Task t: list) {
            System.out.println(t.toString());
        }
        manager.exit();
    }

}
