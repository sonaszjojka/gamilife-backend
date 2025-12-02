package edu.pjwstk.groups.usecase.resendmail;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record ResendMailToGroupInvitationCommand(
        UUID groupId, UUID groupInvitationId
) implements Command {
    @Override
    public void validate() {
        // Validation in API layer
    }
}
