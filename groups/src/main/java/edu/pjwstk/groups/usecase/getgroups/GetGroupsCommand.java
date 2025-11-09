package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.core.Command;

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
