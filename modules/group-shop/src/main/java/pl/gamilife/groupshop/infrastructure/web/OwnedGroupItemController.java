package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsCommand;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsResult;
import pl.gamilife.groupshop.application.getownedgroupitems.GetOwnedGroupItemsUseCase;
import pl.gamilife.groupshop.domain.model.projection.ApiResponse;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemResult;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemResult;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemUseCase;
import pl.gamilife.groupshop.infrastructure.web.request.CreateOwnedGroupItemRequest;
import pl.gamilife.groupshop.infrastructure.web.request.EditOwnedGroupItemRequest;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{memberId}/inventory")
public class OwnedGroupItemController {
    private final CreateOwnedGroupItemUseCase createOwnedGroupItemUseCase;
    private final DeleteOwnedGroupItemUseCase deleteOwnedGroupItemUseCase;
    private final EditOwnedGroupItemUseCase editOwnedGroupItemUseCase;
    private final GetOwnedGroupItemsUseCase getOwnedGroupItemsUseCase;


    @PostMapping("")
    public ResponseEntity<CreateOwnedGroupItemResult> createOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                                           @PathVariable(name = "memberId") UUID memberId,
                                                                           @CurrentUserId UUID currentUserId,
                                                                           @RequestBody @Valid CreateOwnedGroupItemRequest request) {

        CreateOwnedGroupItemCommand cmd = new CreateOwnedGroupItemCommand(
                request.groupItemId(),
                groupId,
                memberId,
                currentUserId
        );

        CreateOwnedGroupItemResult response = createOwnedGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{ownedGroupItemId}")
    public ResponseEntity<ApiResponse> deleteOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                            @PathVariable(name = "memberId") UUID memberId,
                                                            @CurrentUserId UUID currentUserId,
                                                            @PathVariable(name = "ownedGroupItemId") UUID ownedGroupItemId) {
        DeleteOwnedGroupItemCommand cmd = new DeleteOwnedGroupItemCommand( ownedGroupItemId, groupId, memberId, currentUserId);

        deleteOwnedGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(new ApiResponse("Owned group item with id: " + ownedGroupItemId + " deleted successfully."));
    }

    @PutMapping("/{ownedGroupItemId}")
    public ResponseEntity<EditOwnedGroupItemResult> editOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                                       @PathVariable(name = "memberId") UUID memberId,
                                                                       @CurrentUserId UUID currentUserId,
                                                                       @PathVariable(name = "ownedGroupItemId") UUID ownedGroupItemId,
                                                                       @RequestBody @Valid EditOwnedGroupItemRequest request) {
        EditOwnedGroupItemCommand cmd = new EditOwnedGroupItemCommand(request.isUsedUp(),ownedGroupItemId, memberId, groupId, currentUserId);

        EditOwnedGroupItemResult response = editOwnedGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @GetMapping("")
    public ResponseEntity<Page<GetOwnedGroupItemsResult>> getOwnedGroupItems(@PathVariable(name = "groupId") UUID groupId,
                                                                 @PathVariable(name = "memberId") UUID memberId,
                                                                 @CurrentUserId UUID currentUserId,
                                                                 @RequestParam Boolean isUsedUp,
                                                                 @RequestParam(name = "page", defaultValue = "0") int page,
                                                                 @RequestParam(name = "size", defaultValue = "10") int size) {
        Page<GetOwnedGroupItemsResult> response = getOwnedGroupItemsUseCase.execute(new GetOwnedGroupItemsCommand(groupId, memberId, isUsedUp, page, size));

        return ResponseEntity.ok(response);
    }

}
