package pl.gamilife.pomodoro.application.findpomodorotaskbytaskid;


import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;

import java.util.UUID;

public interface FindPomodoroTaskByTaskIdUseCase {
    PomodoroItemDto execute(UUID taskId);
}
