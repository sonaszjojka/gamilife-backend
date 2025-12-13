package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.time.LocalTime;

public record EditTaskRequest(
        @Size(min = 1, max = 200, message = "Title cannot exceed 200 characters")
        String title,

        Boolean removeDescription,

        @Size(min = 1, max = 500, message = "Description cannot exceed 500 characters")
        String description,

        @FutureOrPresent
        LocalDate deadlineDate,

        Boolean removeDeadlineTime,

        LocalTime deadlineTime,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed
) {
}