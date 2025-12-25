package pl.gamilife.gamification.application.usecase.getgamificationuser;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetGamificationUserCommand(@NotNull UUID userId) implements Command {
}
