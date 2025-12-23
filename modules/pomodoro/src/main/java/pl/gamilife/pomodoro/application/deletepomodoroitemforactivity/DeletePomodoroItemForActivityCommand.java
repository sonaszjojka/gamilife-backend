package pl.gamilife.pomodoro.application.deletepomodoroitemforactivity;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;
import pl.gamilife.shared.kernel.enums.ActivityType;

import java.util.UUID;

public record DeletePomodoroItemForActivityCommand(
        @NotNull
        ActivityType activityType,

        @NotNull
        UUID activityId
) implements Command {
}
