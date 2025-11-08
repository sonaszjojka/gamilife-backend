package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record EditGroupInvitationStatusCommand(
        UUID groupId,
        UUID groupInvitationId,
        Integer invitationStatusId,
        String token
)
        implements Command {
    @Override
    public void validate() {
        // Validation done in API layer
    }
}
