package pl.gamilife.group.usecase.editgrouprequeststatus;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupRequestStatusCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID groupRequestId,

        @NotNull
        Integer newGroupRequestStatusId
) implements Command {
}
