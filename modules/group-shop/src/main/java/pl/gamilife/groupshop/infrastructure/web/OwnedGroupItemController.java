package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemCommand;
import pl.gamilife.groupshop.domain.model.projection.ApiResponse;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemResult;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemCommand;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemResult;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;


@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{memberId}/inventory")
public class OwnedGroupItemController {
    private final CreateOwnedGroupItemUseCase createOwnedGroupItemUseCase;
    private final DeleteOwnedGroupItemUseCase deleteOwnedGroupItemUseCase;
    private final EditOwnedGroupItemUseCase editOwnedGroupItemUseCase;

    public OwnedGroupItemController(CreateOwnedGroupItemUseCase createOwnedGroupItemUseCase, DeleteOwnedGroupItemUseCase deleteOwnedGroupItemUseCase, EditOwnedGroupItemUseCase editOwnedGroupItemUseCase) {
        this.createOwnedGroupItemUseCase = createOwnedGroupItemUseCase;
        this.deleteOwnedGroupItemUseCase = deleteOwnedGroupItemUseCase;
        this.editOwnedGroupItemUseCase = editOwnedGroupItemUseCase;
    }


    @PostMapping("")
    public ResponseEntity<CreateOwnedGroupItemResult> createOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                                           @PathVariable(name = "memberId") UUID memberId,
                                                                           @CurrentUserId UUID currentUserId,
                                                                           @RequestBody @Valid CreateOwnedGroupItemCommand request) {

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
                                                                       @RequestBody @Valid EditOwnedGroupItemCommand request) {
        EditOwnedGroupItemResult response = editOwnedGroupItemUseCase.execute(request, ownedGroupItemId, memberId, groupId);
        return ResponseEntity.ok(response);
    }

}
