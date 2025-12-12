package pl.gamilife.pomodoro.application.editpomodoroitem;


import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.domain.service.PomodoroItemService;

@Service
@Transactional
@AllArgsConstructor
public class EditPomodoroItemUseCaseImpl implements EditPomodoroItemUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final PomodoroItemService pomodoroItemService;

    @Override
    public EditPomodoroItemResult execute(EditPomodoroItemCommand cmd) {
        PomodoroItem pomodoroItem = pomodoroItemService
                .ensureExistsAndBelongsToUser(cmd.pomodoroId(), cmd.userId(), cmd.zoneId());

        if (cmd.completeCycles() != null) {
            pomodoroItem.increaseCyclesCompleted(cmd.completeCycles());
        } else {
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
}
