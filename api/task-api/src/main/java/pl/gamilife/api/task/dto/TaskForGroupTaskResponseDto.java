package pl.gamilife.api.task.dto;

import lombok.Builder;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

@Builder
public record TaskForGroupTaskResponseDto(
        UUID taskId,
        String title,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        String description
) {
}
