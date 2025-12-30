package pl.gamilife.group.usecase.creategrouprequest;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupRequestCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId
) implements Command {
}
