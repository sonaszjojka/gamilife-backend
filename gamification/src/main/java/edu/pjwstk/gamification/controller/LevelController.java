package edu.pjwstk.gamification.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/levels")
public class LevelController {

    @GetMapping
    public ResponseEntity<String> getLevels() {
        return ResponseEntity.ok("Hello World, Level Controller!");
    }
}
