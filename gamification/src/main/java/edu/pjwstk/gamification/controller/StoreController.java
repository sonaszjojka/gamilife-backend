package edu.pjwstk.gamification.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/store")
public class StoreController {

    @GetMapping("/items")
    public ResponseEntity<String> getFilteredItems() {
        return ResponseEntity.ok("Hello World, Store Controller!");
    }

    @GetMapping("/items/{itemId}")
    public ResponseEntity<String> getItemDetails(@PathVariable UUID itemId) {
        return ResponseEntity.ok(String.format("Hello World, Store Controller! Item ID: %s", itemId));
    }

}
