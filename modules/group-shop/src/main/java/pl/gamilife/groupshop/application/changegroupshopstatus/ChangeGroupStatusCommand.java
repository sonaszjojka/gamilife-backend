package pl.gamilife.groupshop.application.changegroupshopstatus;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ChangeGroupStatusCommand(
        @NotNull
        Boolean isActive,
        @NotNull
        UUID shopId,
        @NotNull
        UUID groupId,
        @NotNull
        UUID userId
)implements Command {
}
