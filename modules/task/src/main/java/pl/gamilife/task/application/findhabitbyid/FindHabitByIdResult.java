package pl.gamilife.task.application.findhabitbyid;

import java.util.UUID;

public record FindHabitByIdResult(UUID userId, boolean canBeWorkedOn) {
}
