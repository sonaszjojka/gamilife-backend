package pl.gamilife.group.usecase.editgroupmember;

import pl.gamilife.shared.kernel.architecture.Command;

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
