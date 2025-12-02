package edu.pjwstk.groups.usecase.creategrouprequest;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record CreateGroupRequestCommand(
        UUID groupId
) implements Command {
    @Override
    public void validate() {
        // Validation in API layer
    }
}
