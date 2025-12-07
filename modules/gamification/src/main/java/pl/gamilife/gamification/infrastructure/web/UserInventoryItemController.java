package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemCommand;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemResult;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemUseCase;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemResult;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import pl.gamilife.gamification.infrastructure.web.request.PurchaseStoreItemRequest;
import pl.gamilife.gamification.infrastructure.web.request.UpdateInventoryItemRequest;

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

    @PatchMapping("/{userInventoryItemId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<EditInventoryItemResult> updateInventoryItem(
            @PathVariable UUID userId,
            @PathVariable UUID userInventoryItemId,
            @RequestBody UpdateInventoryItemRequest request
    ) {
        return ResponseEntity.ok(
                editInventoryItemUseCase.execute(new EditInventoryItemCommand(
                        userId,
                        userInventoryItemId,
                        request.subtractQuantityBy(),
                        request.isEquipped()
                ))
        );
    }

}
