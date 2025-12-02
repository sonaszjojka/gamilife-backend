package pl.gamilife.grouptask.usecase.creategrouptaskmember;

import lombok.Builder;

import java.util.UUID;
@Builder
public record CreateGroupTaskMemberResponse(
        UUID groupTaskMemberId,
        UUID groupMemberId,
        UUID groupTaskId,
        boolean isMarkedDone
) {


}
