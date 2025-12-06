package pl.gamilife.gamification.application.usecase.getallitemrarities;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAllItemRaritiesCommand() implements Command {
    @Override
    public void validate() {

    }
}
