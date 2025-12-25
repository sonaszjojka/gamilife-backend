package pl.gamilife.gamification.application.usecase.rollbackgrouptaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record RollbackGroupTaskCompletionCommand(@NotNull UUID userId) implements Command {
}
