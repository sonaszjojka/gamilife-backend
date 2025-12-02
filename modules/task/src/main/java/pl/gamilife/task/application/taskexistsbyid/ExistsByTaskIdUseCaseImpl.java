package pl.gamilife.task.application.taskexistsbyid;

import pl.gamilife.task.repository.TaskRepository;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class ExistsByTaskIdUseCaseImpl implements ExistsByTaskIdUseCase {

    private final TaskRepository taskRepository;

    public ExistsByTaskIdUseCaseImpl(TaskRepository taskRepository) {
        this.taskRepository = taskRepository;
    }

    @Override
    public Boolean execute(UUID taskId) {
        return taskRepository.existsById(taskId);
    }
}
