package pl.gamilife.gamification.application.usecase.rollbackpomodorotaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record RollbackPomodoroTaskCompletionCommand(@NotNull UUID userId) implements Command {
}
