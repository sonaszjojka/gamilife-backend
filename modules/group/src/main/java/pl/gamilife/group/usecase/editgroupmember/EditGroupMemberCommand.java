package pl.gamilife.group.usecase.editgroupmember;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record EditGroupMemberCommand(
        UUID groupId,
        UUID groupMemberId,
        Integer groupMoney,
        Integer totalEarnedMoney
) implements Command {
    @Override
    public void validate() {

    }
}
