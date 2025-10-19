package edu.pjwstk.groups.usecase.creategroupinvitation;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record CreateGroupInvitationRequest(
        @NotNull(message = "UserId cannot be null")
        UUID userId
) {
}
