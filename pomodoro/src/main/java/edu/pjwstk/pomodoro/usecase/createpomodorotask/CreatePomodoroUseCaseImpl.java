package edu.pjwstk.pomodoro.usecase.createpomodorotask;

import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.pomodoro.domain.PomodoroTask;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class CreatePomodoroUseCaseImpl implements CreatePomodoroUseCase {

    private final PomodoroTaskRepository pomodoroRepository;
    private final CreatePomodoroTaskMapper createPomodoroTaskMapper;
    private final TasksApi tasksProvider;

    public CreatePomodoroUseCaseImpl(PomodoroTaskRepository pomodoroRepository, CreatePomodoroTaskMapper createPomodoroTaskMapper, TasksApi tasksProvider) {
        this.pomodoroRepository = pomodoroRepository;
        this.createPomodoroTaskMapper = createPomodoroTaskMapper;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public CreatePomodoroTaskResponse execute(UUID taskId, CreatePomodoroTaskRequest request) {
        if (request.workCyclesCompleted() > request.workCyclesNeeded()) {
            throw new InvalidPomodoroTaskData("Work cycles completed cannot be larger than work cycles needed!");
        }

        if (pomodoroRepository.existsByTaskId(taskId)) { // todo: to decide if throw 409 conflict or 400 bad request
            throw new InvalidPomodoroTaskData("Task with id:" + taskId + " has already pomodoro task!");
        }

//        if (!tasksProvider.taskExistsByTaskId(taskId)) {
//            throw new TaskNotFoundException("Task with id:" + taskId + " not found!");
//        }

        tasksProvider.findTaskByTaskId(taskId);

        PomodoroTask pomodoroTask = createPomodoroTaskMapper.toEntity(request, UUID.randomUUID(), taskId);
        PomodoroTask savedPomodoroTask = pomodoroRepository.save(pomodoroTask);
        return createPomodoroTaskMapper.toResponse(savedPomodoroTask);
    }
}
