package edu.pjwstk.gamification.usecase.getalluserachievements;

import edu.pjwstk.core.Command;

public record GetAllUserAchievementsCommand(java.util.UUID userId) implements Command {
    @Override
    public void validate() {

    }
}
