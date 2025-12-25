package pl.gamilife.group.usecase.deletegrouprequest;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteGroupRequestCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID groupRequestId
) implements Command {
}
