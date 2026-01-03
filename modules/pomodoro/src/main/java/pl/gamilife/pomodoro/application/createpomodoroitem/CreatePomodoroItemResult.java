package pl.gamilife.pomodoro.application.createpomodoroitem;

import java.time.Instant;
import java.util.UUID;

public record CreatePomodoroItemResult(
        UUID id,
        Integer cyclesRequired,
        Integer cyclesCompleted,
        UUID taskId,
        UUID habitId,
        Instant createdAt,
        boolean canBeWorkedOn
) {
}
