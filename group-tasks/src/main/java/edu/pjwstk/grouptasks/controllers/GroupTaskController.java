package edu.pjwstk.grouptasks.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/group-tasks")
public class GroupTaskController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Group Task Controller!";
    }

}
