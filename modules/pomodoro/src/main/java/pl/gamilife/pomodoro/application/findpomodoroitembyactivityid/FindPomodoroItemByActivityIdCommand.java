package pl.gamilife.pomodoro.application.findpomodoroitembyactivityid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;
import pl.gamilife.shared.kernel.enums.ActivityType;

import java.util.List;
import java.util.UUID;

public record FindPomodoroItemByActivityIdCommand(
        @NotNull
        List<ActivityItem> activityItems
) implements Command {
    public record ActivityItem(UUID activityId, ActivityType type) {
    }
}
