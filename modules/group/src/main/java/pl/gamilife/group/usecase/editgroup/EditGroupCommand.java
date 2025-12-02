package pl.gamilife.group.usecase.editgroup;

import pl.gamilife.infrastructure.core.architecture.Command;

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
