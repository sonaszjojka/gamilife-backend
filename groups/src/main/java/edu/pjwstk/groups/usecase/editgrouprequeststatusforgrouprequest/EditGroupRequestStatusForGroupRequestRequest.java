package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import jakarta.validation.constraints.NotNull;

public record EditGroupRequestStatusForGroupRequestRequest(
        @NotNull(message = "Group Request Status cannot be null!")
        GroupRequestStatusEnum groupRequestStatus
) {
}
