package pl.gamilife.pomodoro.application.findpomodoroitembyactivityid;

import java.util.UUID;

public record FindPomodoroItemByActivityIdResult(
        UUID activityId,
        UUID pomodoroId,
        int cyclesRequired,
        int cyclesCompleted
) {
}
