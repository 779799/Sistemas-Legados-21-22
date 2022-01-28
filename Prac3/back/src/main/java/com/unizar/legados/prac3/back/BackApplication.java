package com.unizar.legados.prac3.back;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class BackApplication {

	public static void main(String[] args) {
		System.setProperty("java.awt.headless", "false");
		ConfigurableApplicationContext ctx =  SpringApplication.run(BackApplication.class, args);
	}

}
