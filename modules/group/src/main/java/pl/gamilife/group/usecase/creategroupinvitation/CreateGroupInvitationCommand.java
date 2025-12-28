package pl.gamilife.group.usecase.creategroupinvitation;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupInvitationCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID userId
) implements Command {
}
