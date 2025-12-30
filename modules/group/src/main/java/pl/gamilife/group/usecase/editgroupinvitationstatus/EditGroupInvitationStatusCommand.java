package pl.gamilife.group.usecase.editgroupinvitationstatus;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupInvitationStatusCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID groupInvitationId,

        @NotNull
        Integer invitationStatusId,

        @NotBlank
        String token
) implements Command {
}
