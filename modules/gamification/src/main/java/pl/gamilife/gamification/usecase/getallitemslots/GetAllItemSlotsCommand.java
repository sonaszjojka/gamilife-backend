package pl.gamilife.gamification.usecase.getallitemslots;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAllItemSlotsCommand() implements Command {
    @Override
    public void validate() {

    }
}
