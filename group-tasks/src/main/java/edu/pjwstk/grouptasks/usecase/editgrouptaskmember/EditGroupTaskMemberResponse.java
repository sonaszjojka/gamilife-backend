package edu.pjwstk.grouptasks.usecase.editgrouptaskmember;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditGroupTaskMemberResponse(
        UUID groupTaskMemberId,
        int groupMemberId,
        UUID groupTaskId,
        boolean isMarkedDone

) {
}
