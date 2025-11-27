package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record CreateGroupInvitationCommand(
        UUID groupId,
        UUID userId
) implements Command {
    @Override
    public void validate() {
        // Validation done in API layer
    }
}
