package pl.gamilife.gamification.application.usecase.getallitemrarities;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetAllItemRaritiesCommand() implements Command {
    @Override
    public void validate() {

    }
}
