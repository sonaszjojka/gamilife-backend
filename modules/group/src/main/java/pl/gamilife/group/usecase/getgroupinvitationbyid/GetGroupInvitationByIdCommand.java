package pl.gamilife.group.usecase.getgroupinvitationbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGroupInvitationByIdCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID groupInvitationId
) implements Command {
}
