package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.controller.request.PurchaseStoreItemRequest;
import edu.pjwstk.gamification.controller.request.UpdateInventoryItemRequest;
import edu.pjwstk.gamification.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import edu.pjwstk.gamification.usecase.purchasestoreitem.PurchaseStoreItemResult;
import edu.pjwstk.gamification.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users/{userId}/inventory/items")
public class UserInventoryController {

    private final PurchaseStoreItemUseCase purchaseStoreItemUseCase;

    @GetMapping
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<String> getUserInventory(
            @PathVariable UUID userId,
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) Integer itemSlot,
            @RequestParam(required = false) Integer rarity
    ) {
        return ResponseEntity.ok(String.format("Inventory of user with ID %s", userId));
    }

    @PostMapping
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<PurchaseStoreItemResult> purchaseStoreItem(
            @PathVariable UUID userId,
            @RequestBody PurchaseStoreItemRequest request
    ) {
        return ResponseEntity.ok(purchaseStoreItemUseCase.execute(
                        new PurchaseStoreItemCommand(userId, request.itemId())
                )
        );
    }

    @PatchMapping("/{itemId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
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
