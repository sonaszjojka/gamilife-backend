package edu.pjwstk.tasks.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
@RequestMapping("/api/tasks")
public class TaskController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Tasks Controller!";
    }
}
