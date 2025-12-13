package pl.gamilife.task.domain.model.filter;

import java.time.LocalDate;
import java.util.UUID;

public record HabitFilter(
        UUID userId,
        String title,
        Integer categoryId,
        Integer difficultyId,
        boolean isAlive,
        LocalDate currentUserDate
) {
}
