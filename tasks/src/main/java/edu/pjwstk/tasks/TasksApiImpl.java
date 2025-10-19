package edu.pjwstk.tasks;

import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.common.tasksApi.dto.TaskDto;
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

    public TasksApiImpl(ExistsByTaskIdUseCase existsByTaskIdUseCase, FindTaskByIdUseCase findTaskByIdUseCase, DeleteTaskUseCase deleteTaskUseCase) {
        this.existsByTaskIdUseCase = existsByTaskIdUseCase;
        this.findTaskByIdUseCase = findTaskByIdUseCase;
        this.deleteTaskUseCase = deleteTaskUseCase;
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
}
