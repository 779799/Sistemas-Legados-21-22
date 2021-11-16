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

    public String getNumber(){
        return this.number;
    }
    public String getName(){
        return this.name ;
    }
    public String getDescription(){
        return this.description ;
    }
    public String getDate(){
        return this.date;
    }

    @Override
    public String toString() {
        return "Task{" +
                "number='" + number + '\'' +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", date='" + date + '\'' +
                '}';
    }
}
