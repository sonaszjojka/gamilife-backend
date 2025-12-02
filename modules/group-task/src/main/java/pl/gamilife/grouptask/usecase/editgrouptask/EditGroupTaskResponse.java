package pl.gamilife.grouptask.usecase.editgrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;
@Builder
public record EditGroupTaskResponse(

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
