package pl.gamilife.gamification.application.usecase.processgrouptaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessGroupTaskCompletionCommand(@NotNull UUID userId, boolean rewardGranted) implements Command {
}
