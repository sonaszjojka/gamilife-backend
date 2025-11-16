package edu.pjwstk.gamification.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/users/{userId}/achievements")
public class userAchievementController {

    @GetMapping
    public ResponseEntity<String> getUserAchievements(@PathVariable UUID userId) {
        return ResponseEntity.ok("Hello World, User Achievement Controller!");
    }
}
