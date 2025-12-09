package pl.gamilife.task.application.edittask;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

@Component
public interface EditTaskUseCase extends UseCase<EditTaskCommand, EditTaskResult> {
}
