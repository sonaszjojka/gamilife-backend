package pl.gamilife.group.usecase.leavegroup;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record LeaveGroupCommand(
        UUID groupId,
        UUID groupMemberId
) implements Command {
    @Override
    public void validate() {

    }
}
