package pl.gamilife.group.usecase.getgrouptypes;

import pl.gamilife.shared.kernel.architecture.Command;

public record GetGroupTypesCommand() implements Command {
    @Override
    public void validate() {

    }
}
