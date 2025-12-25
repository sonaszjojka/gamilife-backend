package pl.gamilife.gamification.application.usecase.processonboardingcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessOnboardingCompletionCommand(@NotNull UUID userId) implements Command {
}
