package com.unizar.legados.prac3.back.controller;

import com.google.gson.Gson;
import com.unizar.legados.prac3.back.model.Program;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import com.unizar.legados.prac3.back.wrapper.Wrapper;
import java.util.List;
import java.util.Locale;

@RestController
@CrossOrigin(origins = "*", methods= {RequestMethod.GET})

public class Controller {
    @GetMapping("/searchByTape/{tape}")
    public ResponseEntity<String> searchByTape(@PathVariable("tape") String tape) {
        Wrapper wrapper = new Wrapper();
        List<Program> list = wrapper.searchByTape(tape);
        wrapper.finish();
        return ResponseEntity.ok(new Gson().toJson(list));
    }

    @GetMapping("/numRegister")
    public ResponseEntity<String> getNumRegister() {
            Wrapper wrapper = new Wrapper();
            String result = ""+ wrapper.viewInfo();
            wrapper.finish();
        return ResponseEntity.ok(result);
    }

    @GetMapping("/searchByName/{name}")
    public ResponseEntity<String> searchByName(@PathVariable("name") String name) {
        Wrapper wrapper = new Wrapper();
        List<Program> list = wrapper.searchByName(name.toUpperCase(Locale.ROOT));
        wrapper.finish();
        return ResponseEntity.ok(new Gson().toJson(list));
    }

}
