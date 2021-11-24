package sample;

public class Task {
    private String number;
    private String name;
    private String description;
    private String date;

    public Task(String number, String name, String description, String date){
        this.number=number;
        this.name=name;
        this.description=description;
        this.date=date;
    }

    @Override
    public String toString() {
        return "Task " + number + ": " + date + " " + name + " " + description;
    }
}
