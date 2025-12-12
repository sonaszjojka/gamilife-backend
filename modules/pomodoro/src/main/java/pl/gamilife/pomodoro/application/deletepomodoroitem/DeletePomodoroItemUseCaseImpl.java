package pl.gamilife.pomodoro.application.deletepomodoroitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.domain.service.PomodoroItemService;

@Service
@Transactional
@AllArgsConstructor
public class DeletePomodoroItemUseCaseImpl implements DeletePomodoroItemUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;
    private final PomodoroItemService pomodoroItemService;

    @Override
    public Void execute(DeletePomodoroItemCommand cmd) {
        PomodoroItem pomodoroItem = pomodoroItemService
                .ensureExistsAndBelongsToUser(cmd.pomodoroId(), cmd.userId(), cmd.zoneId());

        pomodoroItemRepository.deleteByPomodoroTaskId(pomodoroItem.getId());

        return null;
    }

}
