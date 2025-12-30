package pl.gamilife.group.usecase.creategroupinvitation;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupInvitationCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID targetUserId
) implements Command {
}
