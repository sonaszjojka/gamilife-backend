package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.CreateGroupInvitationRequest;
import edu.pjwstk.groups.controller.request.EditGroupInvitationStatusRequest;
import edu.pjwstk.groups.controller.response.ApiResponse;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationCommand;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationResult;
import edu.pjwstk.groups.usecase.creategroupinvitation.CreateGroupInvitationUseCase;
import edu.pjwstk.groups.usecase.deletegroupinvitation.DeleteGroupInvitationUseCase;
import edu.pjwstk.groups.usecase.deletegroupinvitation.DeleteGroupInvitationCommand;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusCommand;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusResult;
import edu.pjwstk.groups.usecase.editgroupinvitationstatus.EditGroupInvitationStatusUseCase;
import edu.pjwstk.groups.usecase.resendmail.ResendMailToGroupInvitationCommand;
import edu.pjwstk.groups.usecase.resendmail.ResendMailToGroupInvitationUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/invitations")
public class GroupInvitationController {

    private final DeleteGroupInvitationUseCase deleteGroupInvitationUseCase;
    private final EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase;
    private final CreateGroupInvitationUseCase createGroupInvitationUseCase;
    private final ResendMailToGroupInvitationUseCase resendMailByInvitationIdUseCase;

    public GroupInvitationController(DeleteGroupInvitationUseCase deleteGroupInvitationUseCase, EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase, CreateGroupInvitationUseCase createGroupInvitationUseCase, ResendMailToGroupInvitationUseCase resendMailByInvitationIdUseCase) {
        this.deleteGroupInvitationUseCase = deleteGroupInvitationUseCase;
        this.editGroupInvitationStatusUseCase = editGroupInvitationStatusUseCase;
        this.createGroupInvitationUseCase = createGroupInvitationUseCase;
        this.resendMailByInvitationIdUseCase = resendMailByInvitationIdUseCase;
    }

    @PostMapping
    private ResponseEntity<CreateGroupInvitationResult> save(@PathVariable("groupId") UUID groupId,
                                                             @RequestBody @Valid CreateGroupInvitationRequest request) {
        CreateGroupInvitationResult response = createGroupInvitationUseCase.execute(new CreateGroupInvitationCommand(
                groupId,
                request.userId()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupInvitationId}/status")
    private ResponseEntity<EditGroupInvitationStatusResult> editInvitationStatusById(
            @PathVariable("groupId") UUID groupId,
            @PathVariable("groupInvitationId") UUID groupInvitationId,
            @RequestBody @Valid EditGroupInvitationStatusRequest request) {
        EditGroupInvitationStatusResult response = editGroupInvitationStatusUseCase.execute(
                new EditGroupInvitationStatusCommand(
                        groupId,
                        groupInvitationId,
                        request.invitationStatusId(),
                        request.token()
                )
        );
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{groupInvitationId}")
    private ResponseEntity<ApiResponse> deleteById(@PathVariable("groupInvitationId") UUID groupInvitationId,
                                                   @PathVariable UUID groupId) {
        deleteGroupInvitationUseCase.execute(new DeleteGroupInvitationCommand(
                groupId,
                groupInvitationId
        ));
        return ResponseEntity.ok(new ApiResponse("Group Invitation with id: " + groupInvitationId + " deleted successfully."));
    }

    @PostMapping("/{groupInvitationId}/resend")
    private ResponseEntity<ApiResponse> resendMailByInvitationId(@PathVariable UUID groupId, @PathVariable UUID groupInvitationId) {
        resendMailByInvitationIdUseCase.execute(new ResendMailToGroupInvitationCommand(groupId, groupInvitationId));
        return ResponseEntity.ok(new ApiResponse("Mail resend successfully."));
    }

}
