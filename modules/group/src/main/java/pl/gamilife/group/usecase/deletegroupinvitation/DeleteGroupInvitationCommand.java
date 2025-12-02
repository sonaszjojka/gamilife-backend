package pl.gamilife.group.usecase.deletegroupinvitation;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record DeleteGroupInvitationCommand(
        UUID groupId,
        UUID groupInvitationId
) implements Command {
    @Override
    public void validate() {
        if (groupId == null) {
            throw new ValidationException("Group ID cannot be null");
        }

        if (groupInvitationId == null) {
            throw new ValidationException("Group Invitation ID cannot be null");
        }
    }
}
