package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.shared.ApiResponse;
import edu.pjwstk.groups.usecase.deletegroupinvitation.DeleteGroupInvitationById;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-invitations")
public class GroupInvitationController {
    private final DeleteGroupInvitationById deleteGroupInvitationById;

    public GroupInvitationController(DeleteGroupInvitationById deleteGroupInvitationById) {
        this.deleteGroupInvitationById = deleteGroupInvitationById;
    }

    @DeleteMapping("/{groupInvitationId}")
    private ResponseEntity<ApiResponse> deleteById(@PathVariable("groupInvitationId") UUID groupInvitationId){
        deleteGroupInvitationById.execute(groupInvitationId);
        return ResponseEntity.ok(new ApiResponse("Group Invitation with id:" + groupInvitationId + " deleted successfully."));
    }
}
