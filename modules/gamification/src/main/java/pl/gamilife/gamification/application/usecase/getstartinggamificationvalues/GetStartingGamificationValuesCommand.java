package pl.gamilife.gamification.application.usecase.getstartinggamificationvalues;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetStartingGamificationValuesCommand() implements Command {
    @Override
    public void validate() {

    }
}
