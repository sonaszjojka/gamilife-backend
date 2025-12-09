package pl.gamilife.task.application.edittask;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.time.LocalDateTime;

public record EditTaskRequest(
        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @FutureOrPresent
        Instant deadline,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed,

        @Size(min = 1, max = 500, message = "Description cannot exceed 200 characters")
        String description
) {

}