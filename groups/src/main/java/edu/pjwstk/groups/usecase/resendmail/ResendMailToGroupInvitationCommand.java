package edu.pjwstk.groups.usecase.resendmail;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record ResendMailToGroupInvitationCommand(
        UUID groupId, UUID groupInvitationId
) implements Command {
    @Override
    public void validate() {
        // Validation in API layer
    }
}
