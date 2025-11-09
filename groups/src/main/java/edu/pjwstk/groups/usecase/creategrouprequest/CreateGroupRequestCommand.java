package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record CreateGroupRequestCommand(
        UUID groupId
) implements Command {
    @Override
    public void validate() {
        // Validation in API layer
    }
}
