package edu.pjwstk.pomodoro.createpomodorotask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreatePomodoroTaskResponse(
        UUID pomodoroId,
        Integer workCyclesNeeded,
        Integer workCyclesCompleted,
        Instant createdAt,
        UUID taskId
) {
}
