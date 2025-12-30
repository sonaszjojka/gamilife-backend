package pl.gamilife.gamification.application.usecase.rollbackgrouptaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record RollbackGroupTaskCompletionCommand(@NotNull Collection<UUID> userIds) implements Command {
}
