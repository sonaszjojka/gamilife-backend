package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetStoreItemDetailsCommand(
        @NotNull UUID itemId,
        @NotNull UUID userId

) implements Command {
}
