package edu.pjwstk.grouptasks.usecase.getgrouptasks;

import lombok.Builder;
import java.util.UUID;

@Builder
public record GetGroupTaskMemberDto(
        UUID groupTaskMemberId,
        UUID groupMemberId,
        Boolean isMarkedDone

        ) {



}
