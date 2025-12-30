package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemResult;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsCommand;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsResult;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsUseCase;
import pl.gamilife.groupshop.application.purchasegroupitem.PurchaseGroupItemCommand;
import pl.gamilife.groupshop.application.purchasegroupitem.PurchaseGroupItemResult;
import pl.gamilife.groupshop.application.purchasegroupitem.PurchaseGroupItemUseCase;
import pl.gamilife.groupshop.infrastructure.web.request.PurchaseGroupItemRequest;
import pl.gamilife.groupshop.infrastructure.web.request.EditOwnedGroupItemRequest;
import pl.gamilife.groupshop.infrastructure.web.response.ApiResponse;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{memberId}/inventory")
public class OwnedGroupItemController {

    private final PurchaseGroupItemUseCase purchaseGroupItemUseCase;
    private final DeleteOwnedGroupItemUseCase deleteOwnedGroupItemUseCase;
    private final EditOwnedGroupItemUseCase editOwnedGroupItemUseCase;
    private final GetOwnedGroupItemsUseCase getOwnedGroupItemsUseCase;


    @PostMapping
    public ResponseEntity<PurchaseGroupItemResult> purchaseGroupItem(
            @PathVariable UUID groupId,
            @PathVariable UUID memberId,
            @CurrentUserId UUID currentUserId,
            @RequestBody @Valid PurchaseGroupItemRequest request
    ) {
        PurchaseGroupItemCommand cmd = new PurchaseGroupItemCommand(
                request.groupItemId(),
                groupId,
                memberId,
                currentUserId
        );

        PurchaseGroupItemResult response = purchaseGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{ownedGroupItemId}")
    public ResponseEntity<ApiResponse> deleteOwnedGroupItem(@PathVariable UUID groupId,
                                                            @PathVariable UUID memberId,
                                                            @CurrentUserId UUID currentUserId,
                                                            @PathVariable UUID ownedGroupItemId) {
        DeleteOwnedGroupItemCommand cmd = new DeleteOwnedGroupItemCommand(
                ownedGroupItemId, groupId, memberId, currentUserId
        );

        deleteOwnedGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(new ApiResponse(String.format(
                "Owned group item with id: %s deleted successfully.", ownedGroupItemId
        )));
    }

    @PutMapping("/{ownedGroupItemId}")
    public ResponseEntity<EditOwnedGroupItemResult> editOwnedGroupItem(
            @PathVariable UUID groupId,
            @PathVariable UUID memberId,
            @CurrentUserId UUID currentUserId,
            @PathVariable UUID ownedGroupItemId,
            @RequestBody @Valid EditOwnedGroupItemRequest request
    ) {
        EditOwnedGroupItemCommand cmd = new EditOwnedGroupItemCommand(
                request.isUsedUp(), ownedGroupItemId, memberId, groupId, currentUserId
        );

        EditOwnedGroupItemResult response = editOwnedGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<Page<GetOwnedGroupItemsResult>> getOwnedGroupItems(
            @PathVariable UUID groupId,
            @PathVariable UUID memberId,
            @CurrentUserId UUID userId,
            @RequestParam(required = false) Boolean isUsedUp,
            @RequestParam(name = "page", defaultValue = "0") int page,
            @RequestParam(name = "size", defaultValue = "10") int size
    ) {
        Page<GetOwnedGroupItemsResult> response = getOwnedGroupItemsUseCase.execute(new GetOwnedGroupItemsCommand(
                groupId,
                memberId,
                userId,
                isUsedUp,
                page,
                size
        ));

        return ResponseEntity.ok(response);
    }

}
