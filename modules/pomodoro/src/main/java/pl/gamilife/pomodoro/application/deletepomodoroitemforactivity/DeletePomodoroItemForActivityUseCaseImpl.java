package pl.gamilife.pomodoro.application.deletepomodoroitemforactivity;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;

import java.util.Optional;

@Service
@Transactional
@AllArgsConstructor
public class DeletePomodoroItemForActivityUseCaseImpl implements DeletePomodoroItemForActivityUseCase {

    private final PomodoroItemRepository pomodoroItemRepository;

    @Override
    public Boolean execute(DeletePomodoroItemForActivityCommand cmd) {
        Optional<PomodoroItem> possiblePomodoroItem = switch (cmd.activityType()) {
            case TASK -> pomodoroItemRepository.findById(cmd.activityId());
            case HABIT -> pomodoroItemRepository.findById(cmd.activityId());
        };

        if (possiblePomodoroItem.isEmpty()) {
            return false;
        }

        PomodoroItem pomodoroItem = possiblePomodoroItem.get();
        pomodoroItemRepository.deleteByPomodoroTaskId(pomodoroItem.getId());

        return true;
    }
}
