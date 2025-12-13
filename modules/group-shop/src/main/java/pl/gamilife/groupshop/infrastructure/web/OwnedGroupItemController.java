package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.domain.model.projection.ApiResponse;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemRequest;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemResponse;
import pl.gamilife.groupshop.application.createownedgroupitem.CreateOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.deleteownedgroupitem.DeleteOwnedGroupItemUseCase;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemRequest;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemResponse;
import pl.gamilife.groupshop.application.editownedgroupitem.EditOwnedGroupItemUseCase;

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
    public ResponseEntity<CreateOwnedGroupItemResponse> createOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                                             @PathVariable(name = "memberId") UUID memberId,
                                                                             @RequestBody @Valid CreateOwnedGroupItemRequest request) {

        CreateOwnedGroupItemResponse response = createOwnedGroupItemUseCase.execute(request, memberId, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{ownedGroupItemId}")
    public ResponseEntity<ApiResponse> deleteOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                            @PathVariable(name = "memberId") UUID memberId,
                                                            @PathVariable(name = "ownedGroupItemId") UUID ownedGroupItemId) {
        deleteOwnedGroupItemUseCase.execute(groupId, memberId, ownedGroupItemId);
        return ResponseEntity.ok(new ApiResponse("Owned group item with id: " + ownedGroupItemId + " deleted successfully."));
    }

    @PutMapping("/{ownedGroupItemId}")
    public ResponseEntity<EditOwnedGroupItemResponse> editOwnedGroupItem(@PathVariable(name = "groupId") UUID groupId,
                                                                         @PathVariable(name = "memberId") UUID memberId,
                                                                         @PathVariable(name = "ownedGroupItemId") UUID ownedGroupItemId,
                                                                         @RequestBody @Valid EditOwnedGroupItemRequest request) {
        EditOwnedGroupItemResponse response = editOwnedGroupItemUseCase.execute(request, ownedGroupItemId, memberId, groupId);
        return ResponseEntity.ok(response);
    }

}
