package pl.gamilife.api.task.dto;

import jakarta.validation.constraints.FutureOrPresent;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.time.LocalDateTime;

public record TaskForGroupTaskRequestDto(
        @Size(min = 1, max = 200)
        String title,

        @FutureOrPresent
        Instant deadline,

        Integer categoryId,

        Integer difficultyId,

        Boolean completed,

        @Size(min = 1, max = 200)
        String description
) {
}
