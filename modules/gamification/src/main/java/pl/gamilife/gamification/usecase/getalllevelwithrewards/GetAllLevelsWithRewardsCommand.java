package pl.gamilife.gamification.usecase.getalllevelwithrewards;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAllLevelsWithRewardsCommand() implements Command {
    @Override
    public void validate() {

    }
}
