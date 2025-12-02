package pl.gamilife.api.task.dto;

import lombok.Builder;

import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record   TaskForGroupTaskResponseDto(

        UUID taskId,
        String title,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        String description

) {
}
