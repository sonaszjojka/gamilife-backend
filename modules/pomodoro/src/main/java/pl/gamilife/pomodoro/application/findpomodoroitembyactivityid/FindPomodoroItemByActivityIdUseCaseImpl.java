package pl.gamilife.pomodoro.application.findpomodoroitembyactivityid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.enums.ActivityType;

import java.util.List;
import java.util.UUID;

@Service
@AllArgsConstructor
public class FindPomodoroItemByActivityIdUseCaseImpl implements FindPomodoroItemByActivityIdUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;

    @Override
    public List<FindPomodoroItemByActivityIdResult> execute(FindPomodoroItemByActivityIdCommand cmd) {
        List<UUID> taskIds = cmd.activityItems().stream()
                .filter(ai -> ai.type() == ActivityType.TASK)
                .map(FindPomodoroItemByActivityIdCommand.ActivityItem::activityId)
                .toList();

        List<UUID> habitIds = cmd.activityItems().stream()
                .filter(ai -> ai.type() == ActivityType.HABIT)
                .map(FindPomodoroItemByActivityIdCommand.ActivityItem::activityId)
                .toList();

        List<PomodoroItem> result = pomodoroItemRepository.findAllByActivityIdIn(taskIds, habitIds);

        return result.stream()
                .map(p -> new FindPomodoroItemByActivityIdResult(
                        p.getTaskId() != null ? p.getTaskId() : p.getHabitId(),
                        p.getId(),
                        p.getCyclesRequired(),
                        p.getCyclesCompleted()
                ))
                .toList();
    }
}
