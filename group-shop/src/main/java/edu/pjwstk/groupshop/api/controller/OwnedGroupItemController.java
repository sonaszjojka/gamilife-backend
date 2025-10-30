package edu.pjwstk.groupshop.api.controller;

import edu.pjwstk.groupshop.shared.ApiResponse;
import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemRequest;
import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemResponse;
import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemUseCase;
import edu.pjwstk.groupshop.usecase.deleteownedgroupitem.DeleteOwnedGroupItemUseCase;
import edu.pjwstk.groupshop.usecase.editownedgroupitem.EditOwnedGroupItemRequest;
import edu.pjwstk.groupshop.usecase.editownedgroupitem.EditOwnedGroupItemResponse;
import edu.pjwstk.groupshop.usecase.editownedgroupitem.EditOwnedGroupItemUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
    public ResponseEntity<CreateOwnedGroupItemResponse> createOwnedGroupItem(@PathVariable (name="groupId") UUID groupId,
                                                                             @PathVariable (name="memberId") UUID memberId,
                                                                             @RequestBody @Valid CreateOwnedGroupItemRequest request) {

        CreateOwnedGroupItemResponse response= createOwnedGroupItemUseCase.execute(request, memberId,groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response) ;
    }

    @DeleteMapping("/{ownedGroupItemId}")
    public ResponseEntity<ApiResponse> deleteOwnedGroupItem(@PathVariable (name="groupId") UUID groupId,
                                                            @PathVariable (name="memberId") UUID memberId,
                                                            @PathVariable (name="ownedGroupItemId") UUID ownedGroupItemId) {
        deleteOwnedGroupItemUseCase.execute( groupId, memberId, ownedGroupItemId);
        return ResponseEntity.ok(new ApiResponse("Owned group item with id: " + ownedGroupItemId + " deleted successfully."));
    }

    @PutMapping("/{ownedGroupItemId}")
    public ResponseEntity<EditOwnedGroupItemResponse> editOwnedGroupItem(@PathVariable (name="groupId") UUID groupId,
                                                                         @PathVariable (name="memberId") UUID memberId,
                                                                         @PathVariable (name="ownedGroupItemId") UUID ownedGroupItemId,
                                                                         @RequestBody @Valid EditOwnedGroupItemRequest request)
    {
        EditOwnedGroupItemResponse response = editOwnedGroupItemUseCase.execute(request, ownedGroupItemId, groupId, memberId);
        return ResponseEntity.ok(response);
    }

}
