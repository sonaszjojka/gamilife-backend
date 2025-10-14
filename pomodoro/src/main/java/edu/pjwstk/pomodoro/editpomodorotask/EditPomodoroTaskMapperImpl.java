package edu.pjwstk.pomodoro.editpomodorotask;

import edu.pjwstk.pomodoro.entity.PomodoroTask;
import org.springframework.stereotype.Component;

@Component
public class EditPomodoroTaskMapperImpl implements EditPomodoroTaskMapper {

    @Override
    public EditPomodoroTaskResponse toResponse(PomodoroTask pomodoroTask) {
        return EditPomodoroTaskResponse.builder()
                .pomodoroId(pomodoroTask.getPomodoroId())
                .workCyclesNeeded(pomodoroTask.getWorkCyclesNeeded())
                .workCyclesCompleted(pomodoroTask.getWorkCyclesCompleted())
                .createdAt(pomodoroTask.getCreatedAt())
                .taskId(pomodoroTask.getTaskId())
                .build();
    }
}
