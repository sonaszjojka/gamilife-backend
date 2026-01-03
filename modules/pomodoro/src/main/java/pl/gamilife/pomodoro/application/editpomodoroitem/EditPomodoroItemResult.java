package pl.gamilife.pomodoro.application.editpomodoroitem;

import java.time.Instant;
import java.util.UUID;

public record EditPomodoroItemResult(
        UUID id,
        Integer cyclesRequired,
        Integer cyclesCompleted,
        Instant createdAt,
        UUID taskId,
        UUID habitId
) {
}
