package edu.pjwstk.pomodoro.editpomodorotask;

import edu.pjwstk.pomodoro.entity.PomodoroTask;

public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
