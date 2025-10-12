package edu.pjwstk.tasks;

import edu.pjwstk.tasks.application.findtaskbyid.FindTaskByIdUseCase;
import edu.pjwstk.tasks.application.taskexistsbyid.ExistsByTaskIdUseCase;
import edu.pjwstk.tasks.shared.TaskDto;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class TasksApiImpl implements TasksApi {

    private final ExistsByTaskIdUseCase existsByTaskIdUseCase;
    private final FindTaskByIdUseCase findTaskByIdUseCase;

    public TasksApiImpl(ExistsByTaskIdUseCase existsByTaskIdUseCase, FindTaskByIdUseCase findTaskByIdUseCase) {
        this.existsByTaskIdUseCase = existsByTaskIdUseCase;
        this.findTaskByIdUseCase = findTaskByIdUseCase;
    }

    @Override
    public Boolean taskExistsByTaskId(UUID taskId) {
        return existsByTaskIdUseCase.execute(taskId);
    }

    @Override
    public TaskDto findTaskByTaskId(UUID taskId) {
        return findTaskByIdUseCase.execute(taskId);
    }
}
