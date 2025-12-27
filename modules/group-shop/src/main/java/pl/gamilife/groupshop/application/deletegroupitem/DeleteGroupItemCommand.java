package pl.gamilife.groupshop.application.deletegroupitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteGroupItemCommand(
        @NotNull
        UUID groupItemId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID currentUserId
) implements Command {
}
