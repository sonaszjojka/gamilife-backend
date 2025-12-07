package pl.gamilife.gamification.application.usecase.getalluserachievements;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetAllUserAchievementsCommand(UUID userId) implements Command {
    @Override
    public void validate() {

    }
}
