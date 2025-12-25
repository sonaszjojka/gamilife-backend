package pl.gamilife.group.usecase.deletegroupinvitation;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteGroupInvitationCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID groupInvitationId
) implements Command {
}
