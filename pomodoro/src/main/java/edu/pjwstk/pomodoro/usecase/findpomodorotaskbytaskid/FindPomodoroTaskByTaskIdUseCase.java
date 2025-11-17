package edu.pjwstk.pomodoro.usecase.findpomodorotaskbytaskid;

import edu.pjwstk.common.pomodoroApi.dto.PomodoroTaskDto;

import java.util.UUID;

public interface FindPomodoroTaskByTaskIdUseCase {

    public PomodoroTaskDto execute(UUID taskId);
}
