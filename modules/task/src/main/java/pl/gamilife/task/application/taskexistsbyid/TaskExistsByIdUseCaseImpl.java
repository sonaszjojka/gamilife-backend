package pl.gamilife.task.application.taskexistsbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.util.UUID;

@Service
@AllArgsConstructor
public class TaskExistsByIdUseCaseImpl implements TaskExistsByIdUseCase {

    private final TaskRepository taskRepository;

    @Override
    public Boolean execute(TaskExistsByIdCommand cmd) {
        return taskRepository.existsById(cmd.taskId());
    }
}
