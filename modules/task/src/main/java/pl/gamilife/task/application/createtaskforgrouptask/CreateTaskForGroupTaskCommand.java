package pl.gamilife.task.application.createtaskforgrouptask;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;

public record CreateTaskForGroupTaskCommand(
        @NotBlank
        @Size(min = 1, max = 200)
        String title,

        @NotNull
        @FutureOrPresent
        LocalDate deadlineDate,

        LocalTime deadlineTime,

        @NotNull
        ZoneId currentGroupTimezone,

        @NotNull
        Integer categoryId,

        @NotNull
        Integer difficultyId,

        @Size(min = 1, max = 200)
        String description
) implements Command {
}
