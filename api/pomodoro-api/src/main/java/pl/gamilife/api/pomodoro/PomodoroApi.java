package pl.gamilife.api.pomodoro;



import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;

import java.util.UUID;

public interface PomodoroApi {
    public PomodoroTaskDto findPomodoroTaskByTaskId(UUID taskId);
}
