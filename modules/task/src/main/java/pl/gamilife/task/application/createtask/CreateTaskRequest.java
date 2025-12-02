package pl.gamilife.task.application.createtask;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDateTime;

public record CreateTaskRequest(
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
