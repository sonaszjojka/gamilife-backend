package edu.pjwstk.groups.usecase.getgroups.getbyid;

import edu.pjwstk.core.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetGroupByIdCommand(
        UUID groupId
) implements Command, Serializable {
    @Override
    public void validate() {

    }
}
