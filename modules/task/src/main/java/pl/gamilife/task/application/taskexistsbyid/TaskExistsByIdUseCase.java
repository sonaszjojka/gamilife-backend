package pl.gamilife.task.application.taskexistsbyid;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.UUID;

@Component
public interface TaskExistsByIdUseCase extends UseCase<TaskExistsByIdCommand, Boolean> {
}
