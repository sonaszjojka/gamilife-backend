package pl.gamilife.api.pomodoro.dto;

import lombok.Builder;

import java.util.UUID;

@Builder
public record PomodoroTaskDto(
        UUID pomodoroId,
        Integer workCyclesNeeded,
        Integer workCyclesCompleted
) {
}
