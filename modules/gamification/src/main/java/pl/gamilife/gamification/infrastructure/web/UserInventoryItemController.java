package pl.gamilife.gamification.infrastructure.web;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemCommand;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemResult;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemUseCase;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsCommand;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsResult;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsUseCase;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemResult;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import pl.gamilife.gamification.infrastructure.web.request.PurchaseStoreItemRequest;
import pl.gamilife.gamification.infrastructure.web.request.UpdateInventoryItemRequest;
import pl.gamilife.gamification.infrastructure.web.request.UserInventoryItemFilterRequest;
import pl.gamilife.shared.web.security.annotation.AuthenticatedUserIsOwner;

import java.util.UUID;

@RestController
@AllArgsConstructor
@AuthenticatedUserIsOwner
@RequestMapping("/api/v1/users/{userId}/inventory/items")
public class UserInventoryItemController {

    private final PurchaseStoreItemUseCase purchaseStoreItemUseCase;
    private final EditInventoryItemUseCase editInventoryItemUseCase;
    private final GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    @GetMapping
    //TODO: isProfilePrivate
    public ResponseEntity<GetUserInventoryItemsResult> getUserInventoryItems(
            @PathVariable UUID userId,
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) Integer itemSlot,
            @RequestParam(required = false) Integer rarity,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "6") @Min(1) @Max(100) Integer size
    ) {
        UserInventoryItemFilterRequest request = new UserInventoryItemFilterRequest(
                itemName, itemSlot, rarity, page, size
        );
        GetUserInventoryItemsResult response = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(
                        userId,
                        request.itemName(),
                        request.itemSlot(),
                        request.rarity(),
                        request.page(),
                        request.size()
                )
        );
        return ResponseEntity.ok(response);
    }

    @PostMapping
    @AuthenticatedUserIsOwner
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
    @AuthenticatedUserIsOwner
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
