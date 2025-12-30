package pl.gamilife.groupshop.application.editgroupshop;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupShopCommand(
        String name,

        String description,

        @NotNull
        UUID groupId,

        @NotNull
        UUID userId
) implements Command {
}
