package pl.gamilife.task.application.taskexistsbyid;

import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface ExistsByTaskIdUseCase {
    Boolean execute(UUID taskId);
}
