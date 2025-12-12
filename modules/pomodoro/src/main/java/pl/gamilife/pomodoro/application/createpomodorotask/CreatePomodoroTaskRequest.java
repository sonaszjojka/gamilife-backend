package pl.gamilife.pomodoro.application.createpomodorotask;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.util.UUID;

public record CreatePomodoroTaskRequest(

        UUID taskId,

        UUID habitId,

        @NotNull
        @Positive
        Integer workCyclesNeeded
) {
}
