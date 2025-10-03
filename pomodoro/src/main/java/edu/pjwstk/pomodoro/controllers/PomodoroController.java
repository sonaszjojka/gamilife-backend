package edu.pjwstk.pomodoro.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/pomodoro")
public class PomodoroController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Pomodoro Controller!";
    }
}
