package pl.gamilife.gamification.usecase.getalluserachievements;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAllUserAchievementsCommand(java.util.UUID userId) implements Command {
    @Override
    public void validate() {

    }
}
