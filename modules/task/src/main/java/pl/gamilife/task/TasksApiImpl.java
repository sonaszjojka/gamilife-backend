package pl.gamilife.task;

import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskUseCase;
import pl.gamilife.task.application.deletetask.DeleteTaskUseCase;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdUseCase;
import pl.gamilife.task.application.taskexistsbyid.ExistsByTaskIdUseCase;

import java.util.UUID;

@Service
public class TasksApiImpl implements TasksApi {

    private final ExistsByTaskIdUseCase existsByTaskIdUseCase;
    private final FindTaskByIdUseCase findTaskByIdUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;
    private final CreateTaskForGroupTaskUseCase createTaskForGroupTaskUseCase;

    public TasksApiImpl(ExistsByTaskIdUseCase existsByTaskIdUseCase, FindTaskByIdUseCase findTaskByIdUseCase, DeleteTaskUseCase deleteTaskUseCase, CreateTaskForGroupTaskUseCase createTaskForGroupTaskUseCase) {
        this.existsByTaskIdUseCase = existsByTaskIdUseCase;
        this.findTaskByIdUseCase = findTaskByIdUseCase;
        this.deleteTaskUseCase = deleteTaskUseCase;
        this.createTaskForGroupTaskUseCase = createTaskForGroupTaskUseCase;
    }

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

}
