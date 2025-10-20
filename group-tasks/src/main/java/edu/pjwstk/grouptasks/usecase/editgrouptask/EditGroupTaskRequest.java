package edu.pjwstk.grouptasks.usecase.editgrouptask;

import jakarta.validation.constraints.NotNull;


public record EditGroupTaskRequest(
        Integer reward,

        @NotNull
        boolean isAccepted,

        String declineMessage
) {
}
