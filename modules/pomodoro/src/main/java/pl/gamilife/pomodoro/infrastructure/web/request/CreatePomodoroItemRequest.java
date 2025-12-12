package pl.gamilife.pomodoro.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.util.UUID;

public record CreatePomodoroItemRequest(

        UUID taskId,

        UUID habitId,

        @NotNull
        @Positive
        Integer cyclesRequired
) {
}
