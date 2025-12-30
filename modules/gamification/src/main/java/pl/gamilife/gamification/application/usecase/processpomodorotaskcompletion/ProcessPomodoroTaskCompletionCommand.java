package pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessPomodoroTaskCompletionCommand(
        @NotNull
        UUID userId,
        boolean rewardGranted
) implements Command {
}
