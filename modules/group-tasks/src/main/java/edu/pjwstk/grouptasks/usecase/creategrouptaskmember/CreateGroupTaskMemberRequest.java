package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record CreateGroupTaskMemberRequest(
        @NotNull
        UUID groupMemberId
) {
}
