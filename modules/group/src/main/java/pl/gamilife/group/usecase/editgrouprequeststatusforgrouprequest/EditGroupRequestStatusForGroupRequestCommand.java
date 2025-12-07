package pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest;

import pl.gamilife.shared.kernel.architecture.Command;

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
