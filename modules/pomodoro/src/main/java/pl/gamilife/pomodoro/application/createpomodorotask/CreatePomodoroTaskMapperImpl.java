package pl.gamilife.pomodoro.application.createpomodorotask;

import org.springframework.stereotype.Component;
import pl.gamilife.pomodoro.domain.PomodoroItem;

import java.util.UUID;

@Component
public class CreatePomodoroTaskMapperImpl implements CreatePomodoroTaskMapper {
    @Override
    public PomodoroItem toEntity(CreatePomodoroTaskRequest req, UUID pomodoroId, UUID taskId) {
        return null;
//        return PomodoroTask.builder()
//                .pomodoroId(pomodoroId)
//                .workCyclesNeeded(req.workCyclesNeeded())
//                .workCyclesCompleted(req.workCyclesCompleted())
//                .taskId(taskId)
//                .build();
    }

    @Override
    public CreatePomodoroTaskResponse toResponse(PomodoroItem pomodoroItem) {
        return CreatePomodoroTaskResponse.builder()
                .pomodoroId(pomodoroItem.getId())
                .workCyclesNeeded(pomodoroItem.getCyclesRequired())
                .workCyclesCompleted(pomodoroItem.getCyclesCompleted())
                .createdAt(pomodoroItem.getCreatedAt())
                .taskId(pomodoroItem.getTaskId())
                .build();
    }
}
