package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import jakarta.validation.constraints.NotNull;

public record EditGroupRequestStatusForGroupRequestRequest(
        @NotNull(message = "Group Request Status cannot be null!")
        Integer groupRequestStatusId
) {
}
