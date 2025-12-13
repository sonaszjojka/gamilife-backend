package pl.gamilife.api.pomodoro.dto;

import pl.gamilife.shared.kernel.enums.ActivityType;

import java.util.UUID;

public record ActivityItemDto(
        UUID id,
        ActivityType type
) {
}
