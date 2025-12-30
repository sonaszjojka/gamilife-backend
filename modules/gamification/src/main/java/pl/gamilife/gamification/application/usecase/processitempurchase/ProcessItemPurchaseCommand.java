package pl.gamilife.gamification.application.usecase.processitempurchase;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessItemPurchaseCommand(
        @NotNull
        UUID userId,

        @NotNull
        @Positive
        Integer progressAmount
) implements Command {
}
