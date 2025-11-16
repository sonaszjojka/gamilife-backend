package edu.pjwstk.gamification.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/item-rarities")
public class RarityController {

    @GetMapping
    public ResponseEntity<String> getRarities() {
        return ResponseEntity.ok("Hello World, Rarity Controller!");
    }
}
