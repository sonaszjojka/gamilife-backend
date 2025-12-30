package pl.gamilife.gamification.application.usecase.purchasestoreitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record PurchaseStoreItemCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID itemId
) implements Command {
}
