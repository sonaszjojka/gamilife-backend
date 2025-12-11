package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

public record EditHabitRequest(
        @Size(max = 200, message = "Title cannot exceed 200 characters")
        String title,

        @Size(min = 1, max = 200, message = "Description cannot exceed 500 characters")
        String description,

        Integer categoryId,

        Integer difficultyId,

        @Positive
        Integer cycleLength,

        Boolean iterationCompleted,

        Boolean finished
) {
}
