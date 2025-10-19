package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.shared.ApiResponse;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationRequest;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationResponse;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationUseCase;
import edu.pjwstk.groups.usecase.deletegroupinvitation.DeleteGroupInvitationById;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusRequest;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusResponse;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-invitations")
public class GroupInvitationController {

    private final DeleteGroupInvitationById deleteGroupInvitationById;
    private final EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase;
    private final CreateGroupInvitationUseCase createGroupInvitationUseCase;

    public GroupInvitationController(DeleteGroupInvitationById deleteGroupInvitationById, EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase, CreateGroupInvitationUseCase createGroupInvitationUseCase) {
        this.deleteGroupInvitationById = deleteGroupInvitationById;
        this.editGroupInvitationStatusUseCase = editGroupInvitationStatusUseCase;
        this.createGroupInvitationUseCase = createGroupInvitationUseCase;
    }

    @PostMapping
    private ResponseEntity<CreateGroupInvitationResponse> save(@PathVariable("groupId") UUID groupId,
                                                               @RequestBody @Valid CreateGroupInvitationRequest request) {
        CreateGroupInvitationResponse response = createGroupInvitationUseCase.execute(groupId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupInvitationId}/status")
    private ResponseEntity<EditGroupInvitationStatusResponse> editInvitationStatusById(
            @PathVariable("groupId") UUID groupId,
            @PathVariable("groupInvitationId") UUID groupInvitationId,
            @RequestBody @Valid EditGroupInvitationStatusRequest request) {
        EditGroupInvitationStatusResponse response = editGroupInvitationStatusUseCase.execute(groupInvitationId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{groupInvitationId}")
    private ResponseEntity<ApiResponse> deleteById(@PathVariable("groupInvitationId") UUID groupInvitationId) {
        deleteGroupInvitationById.execute(groupInvitationId);
        return ResponseEntity.ok(new ApiResponse("Group Invitation with id: " + groupInvitationId + " deleted successfully."));
    }
}
