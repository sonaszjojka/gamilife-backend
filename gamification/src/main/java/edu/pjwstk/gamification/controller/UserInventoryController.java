package edu.pjwstk.gamification.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/users/{userId}/inventory/items")
public class UserInventoryController {

    @GetMapping
    public ResponseEntity<String> getUserInventory(@PathVariable UUID userId) {
        return ResponseEntity.ok(String.format("Inventory of user with ID %s", userId));
    }

    @PostMapping
    public ResponseEntity<String> addItem(@PathVariable UUID userId) {
        return ResponseEntity.ok(
                String.format("Item has been added to inventory of user %s.", userId)
        );
    }

    @PatchMapping("/{itemId}")
    public ResponseEntity<String> updateInventoryItem(@PathVariable UUID userId, @PathVariable UUID itemId) {
        return ResponseEntity.ok(
                String.format("Item with ID %s in inventory of user %s has been updated.", itemId, userId)
        );
    }

}
