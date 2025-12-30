package pl.gamilife.group.usecase.editgroup;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupCommand(
        UUID userId,
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
