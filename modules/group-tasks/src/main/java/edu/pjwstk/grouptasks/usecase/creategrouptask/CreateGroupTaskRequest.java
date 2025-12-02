package edu.pjwstk.grouptasks.usecase.creategrouptask;

import jakarta.validation.constraints.*;

import java.time.LocalDateTime;
import java.util.UUID;


public record CreateGroupTaskRequest(

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
        String description


) {
}
