package pl.gamilife.task.application.getusershabits;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record GetUsersHabitsCommand(
        @NotNull
        UUID userId,

        ZoneId zoneId,

        String title,

        Integer categoryId,

        Integer difficultyId,

        Boolean isAlive,

        @NotNull
        @PositiveOrZero
        Integer pageNumber,

        @NotNull
        @Positive
        Integer pageSize
) implements Command {
}
