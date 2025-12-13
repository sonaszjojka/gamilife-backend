package pl.gamilife.task.application.getusersactivityitems;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

public record GetUsersActivityItemsCommand(
        @NotNull
        UUID userId,
        ZoneId zoneId,
        String title,
        Integer categoryId,
        Integer difficultyId,
        LocalDate startDate,
        LocalDate endDate,

        @NotNull
        @PositiveOrZero
        Integer page,

        @Positive
        Integer size
) implements Command {
}
