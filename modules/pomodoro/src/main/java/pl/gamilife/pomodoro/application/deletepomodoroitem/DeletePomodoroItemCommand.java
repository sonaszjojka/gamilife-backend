package pl.gamilife.pomodoro.application.deletepomodoroitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record DeletePomodoroItemCommand(
        @NotNull UUID userId,

        ZoneId zoneId,

        @NotNull UUID pomodoroId
) implements Command {
}
