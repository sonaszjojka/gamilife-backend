package pl.gamilife.grouptask.usecase.editgrouptask;


import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

import java.time.Instant;

public record EditGroupTaskRequest(

        @Positive(message = "Reward must be positive")
        @Max(value = 9999, message = "Reward must not exceed 9999")
        Integer reward,

        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @FutureOrPresent
        Instant deadline,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed,

        @Size(min = 1, max = 200, message = "Description cannot exceed 200 characters")
        String description,

        Boolean isAccepted,

        @Size(min = 1, max = 255, message = "Decline message must not exceed 255 characters")
        String declineMessage

) {
}
