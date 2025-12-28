package pl.gamilife.grouptask.usecase.creategrouptask;

import jakarta.validation.constraints.*;

import java.time.LocalDate;
import java.time.LocalTime;


public record CreateGroupTaskRequest(

        @Positive(message = "Reward must be positive")
        @Max(value = 9999, message = "Reward must not exceed 9999")
        Integer reward,

        @NotBlank(message = "Title cannot be blank")
        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @NotNull(message = "Deadline date cannot be null")
        LocalDate deadlineDate,

        LocalTime deadlineTime,

        @NotNull(message = "Category Id cannot be null")
        Integer categoryId,

        @NotNull(message = "Difficulty Id cannot be null")
        Integer difficultyId,

        @Size(max = 200, message = "Description cannot exceed 200 characters")
        String description


) {
}
