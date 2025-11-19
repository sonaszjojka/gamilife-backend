package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.controller.request.PurchaseStoreItemRequest;
import edu.pjwstk.gamification.controller.request.UpdateInventoryItemRequest;
import edu.pjwstk.gamification.usecase.editinventoryitem.EditInventoryItemCommand;
import edu.pjwstk.gamification.usecase.editinventoryitem.EditInventoryItemResult;
import edu.pjwstk.gamification.usecase.editinventoryitem.EditInventoryItemUseCase;
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
public class UserInventoryItemController {

    private final PurchaseStoreItemUseCase purchaseStoreItemUseCase;
    private final EditInventoryItemUseCase editInventoryItemUseCase;

    @GetMapping
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<String> getUserInventoryItems(
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

    @PatchMapping("/{userInventoryId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<EditInventoryItemResult> updateInventoryItem(
            @PathVariable UUID userId,
            @PathVariable UUID userInventoryId,
            @RequestBody UpdateInventoryItemRequest request
    ) {
        return ResponseEntity.ok(
                editInventoryItemUseCase.execute(new EditInventoryItemCommand(
                        userId,
                        userInventoryId,
                        request.subtractQuantityBy(),
                        request.isEquipped()
                ))
        );
    }

}
