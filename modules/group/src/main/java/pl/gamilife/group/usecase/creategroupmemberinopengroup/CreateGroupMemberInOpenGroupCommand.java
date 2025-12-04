package pl.gamilife.group.usecase.creategroupmemberinopengroup;

import pl.gamilife.infrastructure.core.architecture.Command;

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
