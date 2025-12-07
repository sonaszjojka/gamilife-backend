package pl.gamilife.group.usecase.creategroupinvitation;

import pl.gamilife.shared.kernel.architecture.Command;

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
