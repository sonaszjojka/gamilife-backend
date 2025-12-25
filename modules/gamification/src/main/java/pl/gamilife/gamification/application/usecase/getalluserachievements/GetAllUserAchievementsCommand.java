package pl.gamilife.gamification.application.usecase.getalluserachievements;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetAllUserAchievementsCommand(@NotNull UUID userId) implements Command {
}
