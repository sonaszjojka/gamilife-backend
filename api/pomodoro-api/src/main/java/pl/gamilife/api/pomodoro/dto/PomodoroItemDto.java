package pl.gamilife.api.pomodoro.dto;

import lombok.Builder;

import java.util.UUID;

@Builder
public record PomodoroItemDto(
        UUID activityId,
        UUID pomodoroId,
        Integer cyclesRequired,
        Integer cyclesCompleted
) {
}
