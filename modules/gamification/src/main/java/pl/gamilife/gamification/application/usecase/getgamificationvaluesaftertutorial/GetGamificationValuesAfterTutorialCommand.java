package pl.gamilife.gamification.application.usecase.getgamificationvaluesaftertutorial;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetGamificationValuesAfterTutorialCommand() implements Command {
    @Override
    public void validate() {

    }
}
