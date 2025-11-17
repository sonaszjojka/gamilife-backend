package edu.pjwstk.common.pomodoroApi;

import edu.pjwstk.common.pomodoroApi.dto.PomodoroTaskDto;

import java.util.UUID;

public interface PomodoroTaskApi {
    public PomodoroTaskDto findPomodoroTaskByTaskId(UUID taskId);
}
