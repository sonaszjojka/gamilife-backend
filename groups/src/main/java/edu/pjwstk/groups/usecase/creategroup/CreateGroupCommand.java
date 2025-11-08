package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.core.Command;
import jakarta.validation.constraints.*;

import java.util.UUID;

public record CreateGroupCommand(
        UUID adminId,
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
