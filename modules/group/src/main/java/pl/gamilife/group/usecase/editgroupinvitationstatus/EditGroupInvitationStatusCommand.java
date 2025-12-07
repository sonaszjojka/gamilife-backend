package pl.gamilife.group.usecase.editgroupinvitationstatus;

import pl.gamilife.shared.kernel.architecture.Command;

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
