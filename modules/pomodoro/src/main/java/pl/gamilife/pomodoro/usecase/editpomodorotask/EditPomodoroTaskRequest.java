package pl.gamilife.pomodoro.usecase.editpomodorotask;


import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;

public record EditPomodoroTaskRequest(
        @NotNull(message = "Work cycles needed cannot be null")
        @Positive(message = "Work cycles needed streak must be positive")
        Integer workCyclesNeeded,

        @NotNull(message = "Work cycles completed cannot be null")
        @PositiveOrZero(message = "Work cycles completed streak must be positive")
        Integer workCyclesCompleted
)
{

}
