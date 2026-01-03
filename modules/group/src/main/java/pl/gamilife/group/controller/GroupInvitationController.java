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
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/invitations")
public class GroupInvitationController {

    private final DeleteGroupInvitationUseCase deleteGroupInvitationUseCase;
    private final EditGroupInvitationStatusUseCase editGroupInvitationStatusUseCase;
    private final CreateGroupInvitationUseCase createGroupInvitationUseCase;

    @PostMapping
    public ResponseEntity<CreateGroupInvitationResult> save(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupId,
            @RequestBody @Valid CreateGroupInvitationRequest request
    ) {
        CreateGroupInvitationResult response = createGroupInvitationUseCase.execute(new CreateGroupInvitationCommand(
                userId,
                groupId,
                request.userId()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupInvitationId}/status")
    public ResponseEntity<EditGroupInvitationStatusResult> editInvitationStatusById(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupId,
            @PathVariable UUID groupInvitationId,
            @RequestBody @Valid EditGroupInvitationStatusRequest request) {
        EditGroupInvitationStatusResult response = editGroupInvitationStatusUseCase.execute(
                new EditGroupInvitationStatusCommand(
                        userId,
                        groupId,
                        groupInvitationId,
                        request.invitationStatusId(),
                        request.token()
                )
        );
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{groupInvitationId}")
    public ResponseEntity<ApiResponse> deleteById(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupInvitationId,
            @PathVariable UUID groupId
    ) {
        deleteGroupInvitationUseCase.execute(new DeleteGroupInvitationCommand(
                userId,
                groupId,
                groupInvitationId
        ));
        return ResponseEntity.ok(
                new ApiResponse("Group Invitation with id: " + groupInvitationId + " deleted successfully.")
        );
    }

}
