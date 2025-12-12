package pl.gamilife.api.pomodoro.dto;

import lombok.Builder;

import java.util.UUID;

@Builder
public record PomodoroItemDto(
        UUID pomodoroId,
        Integer cyclesRequired,
        Integer cyclesCompleted
) {
}
