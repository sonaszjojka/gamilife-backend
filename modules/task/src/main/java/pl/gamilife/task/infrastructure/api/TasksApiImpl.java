package pl.gamilife.task.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskUseCase;
import pl.gamilife.task.application.deletetask.DeleteTaskUseCase;
import pl.gamilife.task.application.edittaskforgrouptask.EditTaskForGroupTaskUseCase;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class TasksApiImpl implements TasksApi {

    private final FindTaskByIdUseCase findTaskByIdUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;
    private final CreateTaskForGroupTaskUseCase createTaskForGroupTaskUseCase;
    private final EditTaskForGroupTaskUseCase editTaskForGroupTaskUseCase;

    @Override
    public TaskDto findTaskByTaskId(UUID taskId) {
        return findTaskByIdUseCase.execute(taskId);
    }

    @Override
    public void deleteTaskByTaskId(UUID taskId) {
        deleteTaskUseCase.execute(taskId);
    }

    @Override
    public TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request) {
        return createTaskForGroupTaskUseCase.execute(request);
    }

    @Override
    public TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request, UUID taskId) {

        return editTaskForGroupTaskUseCase.execute(request, taskId);
    }

}
