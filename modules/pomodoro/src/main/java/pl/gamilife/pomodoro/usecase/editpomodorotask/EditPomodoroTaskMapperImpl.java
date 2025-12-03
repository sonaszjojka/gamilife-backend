package pl.gamilife.pomodoro.usecase.editpomodorotask;

import org.springframework.stereotype.Component;
import pl.gamilife.pomodoro.entity.PomodoroTask;

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
