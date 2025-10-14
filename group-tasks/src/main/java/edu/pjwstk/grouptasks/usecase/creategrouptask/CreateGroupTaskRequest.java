package edu.pjwstk.grouptasks.usecase.creategrouptask;

import jakarta.validation.constraints.NotNull;

import java.time.Instant;

public record CreateGroupTaskRequest(


        Integer reward,

        @NotNull
        boolean isAccepted,

        Instant acceptedDate,

        String declineMessage
) {
}
