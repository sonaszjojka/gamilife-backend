package edu.pjwstk.grouptasks.usecase.creategrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;
@Builder
public record CreateGroupTaskResponse(
        UUID groupTaskId,
        UUID taskId,
        UUID groupId,
        Integer reward,
        Boolean isAccepted,
        Instant acceptedDate,
        String declineMessage,
        Instant lastEdit
        ) {

}
