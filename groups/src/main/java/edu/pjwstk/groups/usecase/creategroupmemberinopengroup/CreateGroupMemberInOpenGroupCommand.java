package edu.pjwstk.groups.usecase.creategroupmemberinopengroup;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record CreateGroupMemberInOpenGroupCommand(
        UUID groupId,
        UUID userId
) implements Command {
    @Override
    public void validate() {
        // Validation done in API layer
    }
}
