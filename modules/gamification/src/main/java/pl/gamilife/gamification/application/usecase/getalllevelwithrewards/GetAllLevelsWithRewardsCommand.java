package pl.gamilife.gamification.application.usecase.getalllevelwithrewards;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetAllLevelsWithRewardsCommand() implements Command {
    @Override
    public void validate() {

    }
}
