package pl.gamilife.task.application.taskexistsbyid;

import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.port.repository.TaskRepository;

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
