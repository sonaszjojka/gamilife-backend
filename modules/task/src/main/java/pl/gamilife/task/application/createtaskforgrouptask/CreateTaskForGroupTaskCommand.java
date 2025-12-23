package pl.gamilife.task.application.createtaskforgrouptask;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

public record CreateTaskForGroupTaskCommand(
        @NotBlank
        @Size(min = 1, max = 200)
        String title,

        @NotNull
        @FutureOrPresent
        LocalDate deadlineDate,

        LocalTime deadlineTime,

        @NotNull
        LocalDateTime currentGroupDateTime,

        @NotNull
        Integer categoryId,

        @NotNull
        Integer difficultyId,

        @Size(min = 1, max = 200)
        String description
) implements Command {
}
