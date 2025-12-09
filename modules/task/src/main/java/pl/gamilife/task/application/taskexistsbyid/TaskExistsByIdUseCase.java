package pl.gamilife.task.application.taskexistsbyid;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

@Component
public interface TaskExistsByIdUseCase extends UseCase<TaskExistsByIdCommand, Boolean> {
}
