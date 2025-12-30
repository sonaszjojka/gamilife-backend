package pl.gamilife.gamification.application.usecase.processuserregistered;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessUserRegisteredCommand(@NotNull UUID userId) implements Command {
}
