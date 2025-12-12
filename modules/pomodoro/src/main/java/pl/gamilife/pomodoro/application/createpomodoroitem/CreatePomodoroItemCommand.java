package pl.gamilife.pomodoro.application.createpomodoroitem;

import jakarta.validation.ValidationException;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreatePomodoroItemCommand(

        @NotNull
        UUID userId,

        UUID taskId,

        UUID habitId,

        @NotNull
        @Positive
        Integer cyclesRequired
) implements Command {
    @Override
    public void validate() {
        if (taskId == null && habitId == null) {
            throw new ValidationException("Either taskId or habitId must be provided");
        }

        if (taskId != null && habitId != null) {
            throw new ValidationException("Only one of taskId or habitId must be provided");
        }
    }
}
