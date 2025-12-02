package edu.pjwstk.pomodoro.usecase.createpomodorotask;

import edu.pjwstk.pomodoro.entity.PomodoroTask;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreatePomodoroTaskMapperImpl implements CreatePomodoroTaskMapper {
    @Override
    public PomodoroTask toEntity(CreatePomodoroTaskRequest req, UUID pomodoroId, UUID taskId) {
        return PomodoroTask.builder()
                .pomodoroId(pomodoroId)
                .workCyclesNeeded(req.workCyclesNeeded())
                .workCyclesCompleted(req.workCyclesCompleted())
                .taskId(taskId)
                .build();
    }

    @Override
    public CreatePomodoroTaskResponse toResponse(PomodoroTask pomodoroTask) {
        return CreatePomodoroTaskResponse.builder()
                .pomodoroId(pomodoroTask.getPomodoroId())
                .workCyclesNeeded(pomodoroTask.getWorkCyclesNeeded())
                .workCyclesCompleted(pomodoroTask.getWorkCyclesCompleted())
                .createdAt(pomodoroTask.getCreatedAt())
                .taskId(pomodoroTask.getTaskId())
                .build();
    }
}
