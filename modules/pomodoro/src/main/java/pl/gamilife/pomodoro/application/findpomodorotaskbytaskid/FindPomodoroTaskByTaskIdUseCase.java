package pl.gamilife.pomodoro.application.findpomodorotaskbytaskid;


import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;

import java.util.UUID;

public interface FindPomodoroTaskByTaskIdUseCase {
    PomodoroTaskDto execute(UUID taskId);
}
