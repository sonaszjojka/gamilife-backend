package pl.gamilife.pomodoro.application.undopomodoroitemfortask;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record UndoPomodoroItemForTaskCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId
) implements Command {
}
