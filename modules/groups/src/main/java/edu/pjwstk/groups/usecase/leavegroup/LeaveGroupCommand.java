package edu.pjwstk.groups.usecase.leavegroup;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record LeaveGroupCommand(
        UUID groupId,
        UUID groupMemberId
) implements Command {
    @Override
    public void validate() {

    }
}
