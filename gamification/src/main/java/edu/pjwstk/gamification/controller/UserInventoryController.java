package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.controller.request.PurchaseShopItemRequest;
import edu.pjwstk.gamification.controller.request.UpdateInventoryItemRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/users/{userId}/inventory/items")
public class UserInventoryController {

    @GetMapping
    public ResponseEntity<String> getUserInventory(
            @PathVariable UUID userId,
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) Integer itemSlot,
            @RequestParam(required = false) Integer rarity
    ) {
        return ResponseEntity.ok(String.format("Inventory of user with ID %s", userId));
    }

    @PostMapping
    public ResponseEntity<String> purchaseShopItem(
            @PathVariable UUID userId,
            @RequestBody PurchaseShopItemRequest request
    ) {
        return ResponseEntity.ok(
                String.format("Item has been added to inventory of user %s.", userId)
        );
    }

    @PatchMapping("/{itemId}")
    public ResponseEntity<String> updateInventoryItem(
            @PathVariable UUID userId,
            @PathVariable UUID itemId,
            @RequestBody UpdateInventoryItemRequest request
    ) {
        return ResponseEntity.ok(
                String.format("Item with ID %s in inventory of user %s has been updated.", itemId, userId)
        );
    }

}
