module com.example.demo {
    requires javafx.controls;
    requires javafx.fxml;
    requires java.desktop;


    opens sample to javafx.fxml;
    exports sample;
}