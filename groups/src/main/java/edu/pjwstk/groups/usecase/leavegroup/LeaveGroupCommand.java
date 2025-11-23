package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record LeaveGroupCommand(
        UUID groupId,
        UUID groupMemberId
) implements Command {
    @Override
    public void validate() {

    }
}
