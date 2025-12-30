package pl.gamilife.group.usecase.deletegroup;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteGroupCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId
) implements Command {
}
