package edu.pjwstk.pomodoro.usecase.editpomodorotask;
import edu.pjwstk.pomodoro.domain.PomodoroTask;

public interface EditPomodoroTaskMapper {
    EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask);
}
