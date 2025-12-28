package pl.gamilife.pomodoro.application.undopomodoroitemfortask;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.event.PomodoroTaskUndoneEvent;

import java.util.Optional;

@Service
@AllArgsConstructor
public class UndoPomodoroItemForTaskUseCaseImpl implements UndoPomodoroItemForTaskUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public Void execute(UndoPomodoroItemForTaskCommand cmd) {
        Optional<PomodoroItem> pomodoroItemOptional = pomodoroItemRepository.findByTaskId(cmd.taskId());

        if (pomodoroItemOptional.isEmpty()) {
            // No pomodoro item associated with given task
            return null;
        }

        PomodoroItem pomodoroItem = pomodoroItemOptional.get();
        // Undo only if reward was issued
        if (Boolean.TRUE.equals(pomodoroItem.getRewardIssued())) {
            eventPublisher.publishEvent(new PomodoroTaskUndoneEvent(cmd.userId()));
        }

        return null;
    }
}
