package pl.gamilife.gamification.application.usecase.editinventoryitem;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditInventoryItemCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID userInventoryItemId,

        @Positive
        Integer numberOfSoldItems,

        Boolean isEquipped
) implements Command {
}
