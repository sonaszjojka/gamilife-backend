package edu.pjwstk.grouptasks.usecase.editgrouptask;

import jakarta.validation.constraints.NotNull;

import java.time.Instant;

public record EditGroupTaskRequest(
        Integer reward,

        @NotNull
        boolean isAccepted,

        Instant acceptedDate,

        String declineMessage
) {
}
