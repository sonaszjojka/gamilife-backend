package edu.pjwstk.tasks;

import edu.pjwstk.api.tasks.TasksApi;
import edu.pjwstk.api.tasks.dto.TaskDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskResponseDto;
import edu.pjwstk.tasks.application.createtaskforgrouptask.CreateTaskForGroupTaskUseCase;
import edu.pjwstk.tasks.application.deletetask.DeleteTaskUseCase;
import edu.pjwstk.tasks.application.findtaskbyid.FindTaskByIdUseCase;
import edu.pjwstk.tasks.application.taskexistsbyid.ExistsByTaskIdUseCase;
import org.springframework.stereotype.Service;

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
    public Boolean taskExistsByTaskId(UUID taskId) {
        return existsByTaskIdUseCase.execute(taskId);
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
