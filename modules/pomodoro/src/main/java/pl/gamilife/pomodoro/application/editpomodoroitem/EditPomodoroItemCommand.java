package pl.gamilife.pomodoro.application.editpomodoroitem;


import jakarta.validation.ValidationException;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record EditPomodoroItemCommand(

        @NotNull
        UUID userId,

        ZoneId zoneId,

        @NotNull
        UUID pomodoroId,

        @Positive
        Integer cyclesRequired,

        @Positive
        Integer completeCycles,

        Boolean completed
) implements Command {
    @Override
    public void validate() {
        if (Boolean.TRUE.equals(completed) && (cyclesRequired != null || completeCycles != null)) {
            throw new ValidationException("Cannot change cycles required and complete cycles during completion");
        }

        if (cyclesRequired != null && completeCycles != null) {
            throw new ValidationException("Cannot change cycles required and complete cycles in one go");
        }
    }
}
