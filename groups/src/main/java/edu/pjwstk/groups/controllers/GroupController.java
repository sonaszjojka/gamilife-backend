package edu.pjwstk.groups.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
@RequestMapping("/api/groups")
public class GroupController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Group Controller!";
    }
}
