package sample;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

public class Controller {
    private final String GENERAL = "General";
    private final String SPECIFIC = "Específico";
    private final int MAX_TEXT = 13;
    ObservableList<String> optionList = FXCollections.observableArrayList(GENERAL,SPECIFIC);
    TaskManager manager = new TaskManager();

    @FXML
    private ChoiceBox filterBox;
    @FXML
    private ChoiceBox createBox;
    @FXML
    private TextField nameField;
    @FXML
    private TextField dateField;
    @FXML
    private TextField descriptionField;
    @FXML
    private ListView<Task> taskList = new ListView<Task>();
    @FXML
    private Label nameLabel;
    @FXML
    private Label errorLabel;

    @FXML
    private void initialize(){
        errorLabel.setVisible(false);
        filterBox.setItems(optionList);
        createBox.setItems(optionList);
        manager.start();
        createBox.setValue(GENERAL);
        filterBox.setValue(GENERAL);
    }

    @FXML
    private void updateSpecificTaskList(){
        List<Task> list = manager.viewSpecificTasks();
        taskList.getItems().clear();
        for (Task t: list){
            taskList.getItems().add(t);
        }
    }

    @FXML
    private void updateGeneralTaskList(){
        List<Task> list = manager.viewGeneralTasks();
        taskList.getItems().clear();
        for (Task t: list){
            taskList.getItems().add(t);
        }
    }

    @FXML
    public void exitOnAction(ActionEvent actionEvent){
        manager.exit();
        Platform.exit();
    }

    @FXML
    public void filterOnAction(ActionEvent actionEvent){
        updateList();
    }

    @FXML
    public void createTypeOnAction(ActionEvent actionEvent){
        String type = createBox.getValue().toString();
        if (type.equals(GENERAL)){
            nameLabel.setVisible(false);
            nameField.setVisible(false);
        }else {
            nameLabel.setVisible(true);
            nameField.setVisible(true);
        }
    }

    private boolean validDate(String date){
        try {
            DateFormat df = new SimpleDateFormat("ddMM");
            df.setLenient(true);
            df.parse(date);
            return true;
        } catch(ParseException e){
            return false;
        }
    }

    @FXML
    public void createOnAction(ActionEvent actionEvent){
        String type = createBox.getValue().toString();
        String date = dateField.getText();
        String description = descriptionField.getText();
        if(!validDate(date)){
            alert("Indique una fecha válida");
            return;
        }
        description = description.replaceAll(" ", "_");
        if(description.isEmpty()){
            alert("Indique una descripción");
            return;
        }
        if(description.length()>=MAX_TEXT){
            alert("Indique una descripción de hasta "+MAX_TEXT+" caracteres");
            return;
        }
        if (type.equals(GENERAL)){
            manager.assignGeneralTask(description,date);
        }else if (type.equals(SPECIFIC)){
            String name = nameField.getText();
            name = name.replaceAll(" ", "_");
            if(name.isEmpty()){
                alert("Indique un nombre");
                return;
            }
            if(name.length()>=MAX_TEXT){
                alert("Indique un nombre de hasta "+MAX_TEXT+" caracteres");
                return;
            }
            manager.assignSpecificTask(name,description,date);
        }
        errorLabel.setVisible(false);
        if(getFilterType().equals(type)) updateList();
    }

    private void updateList(){
        String type = getFilterType();
        if (type.equals(GENERAL)){
            updateGeneralTaskList();
        } else {
            updateSpecificTaskList();
        }
    }

    private String getFilterType(){
        return filterBox.getValue().toString();
    }

    private void alert(String text){
        errorLabel.setText(text);
        errorLabel.setVisible(true);
        return;
    }

}
