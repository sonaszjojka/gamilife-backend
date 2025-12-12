package pl.gamilife.pomodoro.application.editpomodorotask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record EditPomodoroTaskResponse(
        UUID pomodoroId,
        Integer workCyclesNeeded,
        Integer workCyclesCompleted,
        Instant createdAt,
        UUID taskId) {
}
