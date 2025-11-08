package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record EditGroupRequestStatusForGroupRequestCommand(
        UUID groupId,
        UUID groupRequestId,
        Integer newGroupRequestStatusId
) implements Command {
    @Override
    public void validate() {
        // Validation done in API layer
    }
}
