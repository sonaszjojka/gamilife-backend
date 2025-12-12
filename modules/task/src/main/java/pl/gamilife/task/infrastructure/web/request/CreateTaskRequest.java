package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.time.LocalTime;

public record CreateTaskRequest(
        @NotBlank
        @Size(max = 200)
        String title,

        @FutureOrPresent
        @NotNull
        LocalDate deadlineDate,

        LocalTime deadlineTime,

        @NotNull
        Integer categoryId,

        @NotNull
        Integer difficultyId,

        @Size(min = 1, max = 200)
        String description
) {
}
