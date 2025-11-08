package edu.pjwstk.groups.usecase.creategroupmemberafteracceptation;

import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.entity.GroupRequest;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.util.Optional;
import java.util.UUID;

@Builder
public record CreateGroupMemberAfterAcceptationRequest(
        @NotNull(message = "UserId cannot be null")
        UUID userId,

        @NotNull(message = "GroupId cannot be null")
        UUID groupId
) {
}
