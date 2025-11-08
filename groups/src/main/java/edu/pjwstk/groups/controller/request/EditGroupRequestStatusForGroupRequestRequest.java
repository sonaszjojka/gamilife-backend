package edu.pjwstk.groups.controller.request;

import jakarta.validation.constraints.NotNull;

public record EditGroupRequestStatusForGroupRequestRequest(
        @NotNull(message = "Group Request Status cannot be null!")
        Integer groupRequestStatusId
) {
}
