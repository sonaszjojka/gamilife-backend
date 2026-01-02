package pl.gamilife.group.usecase.leavegroup;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record LeaveGroupCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID groupMemberId
) implements Command {
}
