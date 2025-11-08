package edu.pjwstk.groups.usecase.creategroupmemberafteracceptation;

import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.util.UUID;

@Builder
public record CreateGroupMemberAfterAcceptationRequest(
        @NotNull(message = "UserId cannot be null")
        UUID userId,

        @NotNull(message = "GroupId cannot be null")
        UUID groupId
) {
}
