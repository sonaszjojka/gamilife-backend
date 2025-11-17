package edu.pjwstk.common.pomodoroApi.dto;

import java.time.Instant;
import java.util.UUID;

public record PomodoroTaskDto(
    UUID pomodoroId,
    Integer workCyclesNeeded,
    Integer workCyclesCompleted,
    Instant createdAt

) {
}
