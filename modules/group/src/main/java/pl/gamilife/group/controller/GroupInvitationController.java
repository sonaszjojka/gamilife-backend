package pl.gamilife.group.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.controller.request.CreateGroupInvitationRequest;
import pl.gamilife.group.controller.request.EditGroupInvitationStatusRequest;
import pl.gamilife.group.controller.response.ApiResponse;
import pl.gamilife.group.usecase.creategroupinvitation.CreateGroupInvitationCommand;
import pl.gamilife.group.usecase.creategroupinvitation.CreateGroupInvitationResult;
import pl.gamilife.group.usecase.creategroupinvitation.CreateGroupInvitationUseCase;
import pl.gamilife.group.usecase.deletegroupinvitation.DeleteGroupInvitationCommand;
import pl.gamilife.group.usecase.deletegroupinvitation.DeleteGroupInvitationUseCase;
import pl.gamilife.group.usecase.editgroupinvitationstatus.EditGroupInvitationStatusCommand;
import pl.gamilife.group.usecase.editgroupinvitationstatus.EditGroupInvitationStatusResult;
import pl.gamilife.group.usecase.editgroupinvitationstatus.EditGroupInvitationStatusUseCase;
import pl.gamilife.group.usecase.resendmail.ResendMailToGroupInvitationCommand;
import pl.gamilife.group.usecase.resendmail.ResendMailToGroupInvitationUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/invitations")
public class GroupInvitationController {

    private final DeleteGroupInvitationUseCase deleteGroupInvitationUseCase;
    private final EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase;
    private final CreateGroupInvitationUseCase createGroupInvitationUseCase;
    private final ResendMailToGroupInvitationUseCase resendMailByInvitationIdUseCase;

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
        return ResponseEntity.ok(
                new ApiResponse("Group Invitation with id: " + groupInvitationId + " deleted successfully.")
        );
    }

    @PostMapping("/{groupInvitationId}/resend")
    private ResponseEntity<ApiResponse> resendMailByInvitationId(@PathVariable UUID groupId,
                                                                 @PathVariable UUID groupInvitationId) {
        resendMailByInvitationIdUseCase.execute(new ResendMailToGroupInvitationCommand(groupId, groupInvitationId));
        return ResponseEntity.ok(new ApiResponse("Mail resend successfully."));
    }

}
