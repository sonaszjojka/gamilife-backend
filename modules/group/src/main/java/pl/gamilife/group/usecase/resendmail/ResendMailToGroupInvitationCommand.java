package pl.gamilife.group.usecase.resendmail;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ResendMailToGroupInvitationCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID groupInvitationId
) implements Command {
}
