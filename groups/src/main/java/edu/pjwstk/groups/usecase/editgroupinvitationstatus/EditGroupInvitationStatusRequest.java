package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.groups.shared.InvitationStatusEnum;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record EditGroupInvitationStatusRequest(

        @NotNull(message = "Invitation status cannot be null!")
        InvitationStatusEnum invitationStatus,

        @NotBlank(message = "Token cannot be blank!")
        String token
) {
}
