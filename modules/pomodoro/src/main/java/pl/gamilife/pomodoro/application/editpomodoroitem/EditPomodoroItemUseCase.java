package pl.gamilife.pomodoro.application.editpomodoroitem;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.architecture.UseCase;

@Component
public interface EditPomodoroItemUseCase extends UseCase<EditPomodoroItemCommand, EditPomodoroItemResult> {
}
