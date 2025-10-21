package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditGroupTaskMemberResponse(
        UUID groupTaskMemberId,
        UUID groupMemberId,
        UUID groupTaskId,
        boolean isMarkedDone

) {
}
