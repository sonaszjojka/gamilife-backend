package edu.pjwstk.grouptasks.usecase.editgrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;
@Builder
public record EditGroupTaskResponse(

        UUID groupTaskId,
        UUID taskId,
        Integer reward,
        boolean isAccepted,
        Instant acceptedDate,
        String declineMessage,
        Instant lastEdit
) {
}
