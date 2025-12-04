package pl.gamilife.grouptask.usecase.editgrouptask;


import jakarta.validation.constraints.*;

import java.time.LocalDateTime;

public record EditGroupTaskRequest(


        @Positive(message = "Reward must be positive")
        @Max(value = 9999, message = "Reward must not exceed 9999")
        Integer reward,


        @NotBlank(message = "Title cannot be blank")
        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @NotNull(message = "Start Time cannot be null")
        LocalDateTime startTime,

        LocalDateTime endTime,

        @NotNull(message = "Category Id cannot be null")
        Integer categoryId,

        @NotNull(message = "Difficulty Id cannot be null")
        Integer difficultyId,

        LocalDateTime completedAt,

        @Size(max = 200, message = "Description cannot exceed 200 characters")
        String description,

        Boolean isAccepted,

        @Size(max = 255, message = "Decline message must not exceed 255 characters")
        String declineMessage

) {
}
