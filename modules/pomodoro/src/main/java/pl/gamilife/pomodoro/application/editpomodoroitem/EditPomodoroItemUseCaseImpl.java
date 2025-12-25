package pl.gamilife.pomodoro.application.editpomodoroitem;


import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.context.TaskContext;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.domain.service.PomodoroItemService;
import pl.gamilife.shared.kernel.event.PomodoroTaskCompletedEvent;

import java.time.ZoneId;
import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditPomodoroItemUseCaseImpl implements EditPomodoroItemUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final PomodoroItemService pomodoroItemService;
    private final ApplicationEventPublisher eventPublisher;
    private final TaskContext taskContext;

    @Override
    public EditPomodoroItemResult execute(EditPomodoroItemCommand cmd) {
        PomodoroItem pomodoroItem = pomodoroItemService
                .ensureExistsAndBelongsToUser(cmd.pomodoroId(), cmd.userId(), cmd.zoneId());

        if (Boolean.TRUE.equals(cmd.completed())) {
            completeAssociatedActivity(pomodoroItem, cmd.userId(), cmd.zoneId());
        } else if (cmd.completeCycles() != null) {
            pomodoroItem.increaseCyclesCompleted(cmd.completeCycles());
        } else if (cmd.cyclesRequired() != null) {
            pomodoroItem.changeCyclesRequired(cmd.cyclesRequired());
        }

        pomodoroItemRepository.save(pomodoroItem);

        return new EditPomodoroItemResult(
                pomodoroItem.getId(),
                pomodoroItem.getCyclesRequired(),
                pomodoroItem.getCyclesCompleted(),
                pomodoroItem.getCreatedAt(),
                pomodoroItem.getTaskId(),
                pomodoroItem.getHabitId()
        );
    }

    private void completeAssociatedActivity(PomodoroItem pomodoroItem, UUID userId, ZoneId zoneId) {
        pomodoroItem.complete();

        if (pomodoroItem.getTaskId() != null) {
            taskContext.completeTaskById(
                    userId,
                    zoneId,
                    pomodoroItem.getTaskId()
            );
        } else {
            taskContext.completeHabitById(
                    userId,
                    zoneId,
                    pomodoroItem.getHabitId()
            );
        }

        eventPublisher.publishEvent(new PomodoroTaskCompletedEvent(userId, pomodoroItem.getRewardIssued()));

        pomodoroItem.markRewardAsIssued();
    }
}
