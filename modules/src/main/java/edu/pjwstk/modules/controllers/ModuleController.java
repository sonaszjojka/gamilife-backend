package edu.pjwstk.modules.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
@RequestMapping("/api/modules")
public class ModuleController {

    @GetMapping
    public String test() {
        return "Hello World, Modules Controller!";
    }
}
