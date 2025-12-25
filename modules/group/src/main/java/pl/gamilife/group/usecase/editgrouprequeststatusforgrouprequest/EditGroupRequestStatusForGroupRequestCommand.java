package pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupRequestStatusForGroupRequestCommand(
        @NotNull
        UUID groupId,

        @NotNull
        UUID groupRequestId,

        @NotNull
        Integer newGroupRequestStatusId
) implements Command {
}
