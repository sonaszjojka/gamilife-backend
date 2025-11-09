package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.core.Command;

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
