package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

@Component
public interface EditHabitUseCase extends UseCase<EditHabitCommand, EditHabitResult> {
}
