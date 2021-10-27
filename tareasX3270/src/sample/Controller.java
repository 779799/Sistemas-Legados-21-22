package sample;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.TextField;

import javax.swing.*;
import javax.xml.soap.Text;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

public class Controller {

    ObservableList<String> tiposList = FXCollections.observableArrayList("General","Específico");

    @FXML
    private ChoiceBox tipoFiltro;


    @FXML
    private ChoiceBox tipoCrear;

    @FXML
    private TextField nombreCrear;
    @FXML
    private TextField fechaCrear;
    @FXML
    private TextField descripcionCrear;

    @FXML
    private void initialize(){
        tipoFiltro.setItems(tiposList);
        tipoCrear.setItems(tiposList);
    }
    @FXML
    public void btSalir(ActionEvent actionEvent){
        System.out.println("Pulsado botón salir");
        Platform.exit();
    }

    @FXML
    public void cbFiltro(ActionEvent actionEvent){
        String tipo = tipoFiltro.getValue().toString();

        if (tipo == "General"){
            System.out.println("Seleccionado filtro general");
            // Muestra lista general


        }else {
            System.out.println("Seleccionado filtro especifico");
            // Muestra lista específicos


        }

    }

    @FXML
    public void cbCrear(ActionEvent actionEvent){
        System.out.println("Seleccionado tipo crear");
        String tipo = tipoCrear.getValue().toString();

        if (tipo.equals("General")){
            nombreCrear.setEditable(false);
        }else {
            nombreCrear.setEditable(true);
        }
    }

    private boolean checkErrorFecha(String fecha){
        try {
            DateFormat df = new SimpleDateFormat("ddMM");
            df.setLenient(false);
            df.parse(fecha);
            return false;
        } catch(ParseException e){
            return true;
        }
    }

    @FXML
    public void btCrear(ActionEvent actionEvent){

        String tipo = tipoCrear.getValue().toString();

        if (tipo.equals("General")){
            System.out.println("Crea tarea general");
            String fecha = fechaCrear.getText();
            String descripcion = descripcionCrear.getText();
            descripcion = descripcion.replaceAll(" ", "_");
            if (checkErrorFecha(fecha)){
                return;
            }else{
                // Introduce tarea general


            }



        }else {
            System.out.println("Crea tarea especifica");
            String nombre = nombreCrear.getText();
            nombre = nombre.replaceAll(" ", "_");
            String fecha = fechaCrear.getText();
            String descripcion = descripcionCrear.getText();
            descripcion = descripcion.replaceAll(" ", "_");
            if (checkErrorFecha(fecha)){
                System.out.println("ERROR");
                return;
            }else{
                // Introduce tarea específica


            }



        }

    }

}
