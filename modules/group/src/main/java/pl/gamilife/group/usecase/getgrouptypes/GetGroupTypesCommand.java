package pl.gamilife.group.usecase.getgrouptypes;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetGroupTypesCommand() implements Command {
    @Override
    public void validate() {

    }
}
