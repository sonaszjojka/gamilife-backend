package pl.gamilife.gamification.application.usecase.processgrouptaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record ProcessGroupTaskCompletionCommand(@NotNull Collection<UUID> userIds,
                                                boolean rewardGranted) implements Command {
}
