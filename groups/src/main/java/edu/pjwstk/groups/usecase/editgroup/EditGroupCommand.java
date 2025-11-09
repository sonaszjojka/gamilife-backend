package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record EditGroupCommand(
        UUID groupId,
        UUID adminId,
        String groupName,
        Character groupCurrencySymbol,
        Integer groupTypeId,
        Integer membersLimit
) implements Command {
    @Override
    public void validate() {

    }
}
