package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import lombok.Builder;

import java.util.UUID;
@Builder
public record CreateGroupTaskMemberResponse(
        UUID groupTaskMemberId,
        int groupMemberId,
        UUID groupTaskId,
        boolean isMarkedDone
) {


}
