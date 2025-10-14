package edu.pjwstk.grouptasks.usecase.creategrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;
@Builder
public record CreateGroupTaskResponse(
        UUID groupTaskId,
        UUID taskId,
        Integer reward,
        boolean isAccepted,
        Instant acceptedDate,
        String declineMessage,
        Instant lastEdit
        ) {

}
