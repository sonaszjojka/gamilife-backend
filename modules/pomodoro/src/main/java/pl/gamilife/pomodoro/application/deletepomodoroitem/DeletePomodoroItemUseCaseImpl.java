package pl.gamilife.pomodoro.application.deletepomodoroitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.exception.PomodoroItemNotFound;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.context.TaskContext;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class DeletePomodoroItemUseCaseImpl implements DeletePomodoroItemUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final TaskContext taskContext;

    @Override
    public Void execute(DeletePomodoroItemCommand cmd) {
        PomodoroItem pomodoroItem = pomodoroItemRepository.findById(cmd.pomodoroId()).orElseThrow(() ->
                new PomodoroItemNotFound("Pomodoro item with id: " + cmd.pomodoroId() + " does not exist"));

        UUID userId = pomodoroItem.getTaskId() != null
                ? taskContext.findTaskById(pomodoroItem.getTaskId()).userId()
                : taskContext.findHabitById(pomodoroItem.getHabitId()).userId();

        if (!userId.equals(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroItem.getTaskId());
        }

        pomodoroItemRepository.deleteByPomodoroTaskId(pomodoroItem.getId());

        return null;
    }

}
