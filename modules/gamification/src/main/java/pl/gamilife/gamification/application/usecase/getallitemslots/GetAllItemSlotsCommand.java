package pl.gamilife.gamification.application.usecase.getallitemslots;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetAllItemSlotsCommand() implements Command {
    @Override
    public void validate() {

    }
}
