package edu.pjwstk.groups.controller.request;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record CreateGroupInvitationRequest(
        @NotNull(message = "UserId cannot be null")
        UUID userId
) {
}
