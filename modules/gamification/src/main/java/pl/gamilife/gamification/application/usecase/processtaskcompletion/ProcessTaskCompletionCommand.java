package pl.gamilife.gamification.application.usecase.processtaskcompletion;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessTaskCompletionCommand(
        @NotNull
        UUID userId,
        boolean rewardGranted
) implements Command {
}
