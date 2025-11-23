package edu.pjwstk.api.pomodoro;



import edu.pjwstk.api.pomodoro.PomodoroTaskDto.PomodoroTaskDto;

import java.util.UUID;

public interface PomodoroApi {
    public PomodoroTaskDto findPomodoroTaskByTaskId(UUID taskId);
}
