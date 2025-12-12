package pl.gamilife.pomodoro.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.infrastructure.persistence.jpa.PomodoroTaskRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class PomodoroItemRepositoryImpl implements PomodoroItemRepository {

    private final PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa;

    @Override
    public boolean existsByTaskId(UUID taskId) {
        return pomodoroTaskRepositoryJpa.existsByTaskId(taskId);
    }

    @Override
    public boolean existsByHabitId(UUID habitId) {
        return pomodoroTaskRepositoryJpa.existsByHabitId(habitId);
    }

    @Override
    public PomodoroItem save(PomodoroItem pomodoroItem) {
        return pomodoroTaskRepositoryJpa.save(pomodoroItem);
    }

    @Override
    public void deleteByPomodoroTaskId(UUID pomodoroItemId) {
        pomodoroTaskRepositoryJpa.deleteById(pomodoroItemId);
    }

    @Override
    public Optional<PomodoroItem> findById(UUID pomodoroItemId) {
        return pomodoroTaskRepositoryJpa.findById(pomodoroItemId);
    }
}
