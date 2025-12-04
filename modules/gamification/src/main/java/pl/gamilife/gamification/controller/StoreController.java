package pl.gamilife.gamification.controller;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/store")
public class StoreController {

    @GetMapping("/items")
    public ResponseEntity<String> getFilteredItems(
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) Integer itemSlot,
            @RequestParam(required = false) Integer rarity,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) Integer size
    ) {
        return ResponseEntity.ok("Hello World, Store Controller!");
    }

    @GetMapping("/items/{itemId}")
    public ResponseEntity<String> getItemDetails(@PathVariable UUID itemId) {
        return ResponseEntity.ok(String.format("Hello World, Store Controller! Item ID: %s", itemId));
    }

}
