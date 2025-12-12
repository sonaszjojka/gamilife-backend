package pl.gamilife.pomodoro.application.deletepomodoroitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeletePomodoroItemCommand(@NotNull UUID userId, @NotNull UUID pomodoroId) implements Command {
}
