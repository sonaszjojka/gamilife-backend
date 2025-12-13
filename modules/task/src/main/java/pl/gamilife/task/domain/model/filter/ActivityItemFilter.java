package pl.gamilife.task.domain.model.filter;

import java.time.LocalDate;
import java.util.UUID;

public record ActivityItemFilter(
        UUID userId,
        String title,
        Integer categoryId,
        Integer difficultyId,
        LocalDate startDate,
        LocalDate endDate
) {
}
