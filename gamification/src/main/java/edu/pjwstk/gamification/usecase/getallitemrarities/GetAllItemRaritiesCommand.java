package edu.pjwstk.gamification.usecase.getallitemrarities;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetAllItemRaritiesCommand() implements Command {
    @Override
    public void validate() {

    }
}
