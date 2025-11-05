package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record EditGroupInvitationStatusRequest(

        @NotNull(message = "Invitation status cannot be null!")
        Integer invitationStatusId,

        @NotBlank(message = "Token cannot be blank!")
        String token
) {
}
