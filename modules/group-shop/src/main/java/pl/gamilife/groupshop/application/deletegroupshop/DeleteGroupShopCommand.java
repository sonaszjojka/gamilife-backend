package pl.gamilife.groupshop.application.deletegroupshop;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteGroupShopCommand(
        @NotNull
        UUID groupId
) implements Command {
}
