package edu.pjwstk.pomodoro.usecase.editpomodorotask;

import edu.pjwstk.pomodoro.entity.PomodoroTask;


public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
