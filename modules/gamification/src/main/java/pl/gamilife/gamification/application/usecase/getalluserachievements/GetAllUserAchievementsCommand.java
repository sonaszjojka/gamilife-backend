package pl.gamilife.gamification.application.usecase.getalluserachievements;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record GetAllUserAchievementsCommand(UUID userId) implements Command {
    @Override
    public void validate() {

    }
}
