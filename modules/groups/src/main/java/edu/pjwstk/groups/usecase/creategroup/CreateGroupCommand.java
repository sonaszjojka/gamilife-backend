package edu.pjwstk.groups.usecase.creategroup;

import pl.gamilife.infrastructure.core.architecture.Command;

public record CreateGroupCommand(
        String groupName,
        Character groupCurrencySymbol,
        Integer groupTypeId,
        Integer membersLimit
) implements Command {
    @Override
    public void validate() {
        // Validation done in API layer
    }
}
