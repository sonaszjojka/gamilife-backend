package pl.gamilife.pomodoro.domain.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.pomodoro.domain.exception.PomodoroItemNotFound;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.context.TaskContext;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.domain.service.PomodoroItemService;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@AllArgsConstructor
public class PomodoroItemServiceImpl implements PomodoroItemService {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final TaskContext taskContext;

    @Override
    public PomodoroItem ensureExistsAndBelongsToUser(UUID pomodoroId, UUID currentUserId) {
        PomodoroItem pomodoroItem = pomodoroItemRepository.findById(pomodoroId).orElseThrow(() ->
                new PomodoroItemNotFound("Pomodoro item with id: " + pomodoroId + " does not exist"));

        UUID userId = pomodoroItem.getTaskId() != null
                ? taskContext.findTaskById(pomodoroItem.getTaskId()).userId()
                : taskContext.findHabitById(pomodoroItem.getHabitId()).userId();

        if (!currentUserId.equals(userId)) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + pomodoroItem.getTaskId());
        }

        return pomodoroItem;
    }
}
