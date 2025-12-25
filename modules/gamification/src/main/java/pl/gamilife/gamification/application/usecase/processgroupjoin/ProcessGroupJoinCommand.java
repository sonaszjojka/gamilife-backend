package pl.gamilife.gamification.application.usecase.processgroupjoin;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessGroupJoinCommand(@NotNull UUID userId, boolean isFirstTimeJoin) implements Command {
}
