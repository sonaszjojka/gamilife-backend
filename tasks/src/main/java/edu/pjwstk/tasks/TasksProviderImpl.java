package edu.pjwstk.tasks;

import edu.pjwstk.tasks.application.findtaskbyid.FindTaskByIdUseCase;
import edu.pjwstk.tasks.application.taskexistsbyid.ExistsByTaskIdUseCase;
import edu.pjwstk.tasks.entity.Task;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
public class TasksProviderImpl implements TasksProvider {

    private final ExistsByTaskIdUseCase existsByTaskIdUseCase;
    private final FindTaskByIdUseCase findTaskByIdUseCase;

    public TasksProviderImpl(ExistsByTaskIdUseCase existsByTaskIdUseCase, FindTaskByIdUseCase findTaskByIdUseCase) {
        this.existsByTaskIdUseCase = existsByTaskIdUseCase;
        this.findTaskByIdUseCase = findTaskByIdUseCase;
    }

    @Override
    public Boolean taskExistsByTaskId(UUID taskId) {
        return existsByTaskIdUseCase.execute(taskId);
    }

    @Override
    public Task findTaskByTaskId(UUID taskId) {
        return findTaskByIdUseCase.execute(taskId);
    }
}
