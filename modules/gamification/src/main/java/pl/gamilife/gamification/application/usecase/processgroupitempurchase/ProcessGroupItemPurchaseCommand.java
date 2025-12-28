package pl.gamilife.gamification.application.usecase.processgroupitempurchase;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessGroupItemPurchaseCommand(@NotNull UUID userId) implements Command {
}
