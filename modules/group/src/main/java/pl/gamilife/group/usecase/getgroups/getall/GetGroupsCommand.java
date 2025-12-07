package pl.gamilife.group.usecase.getgroups.getall;

import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;

public record GetGroupsCommand(
        String joinCode,
        Integer type,
        String name,
        Integer page,
        Integer size
) implements Command, Serializable {
    @Override
    public void validate() {
    }
}
