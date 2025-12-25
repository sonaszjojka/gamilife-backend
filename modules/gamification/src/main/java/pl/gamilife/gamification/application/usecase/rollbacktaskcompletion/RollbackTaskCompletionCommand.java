package pl.gamilife.gamification.application.usecase.rollbacktaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record RollbackTaskCompletionCommand(@NotNull UUID userId) implements Command {
}
