package pl.gamilife.pomodoro.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.pomodoro.domain.PomodoroItem;
import pl.gamilife.pomodoro.domain.repository.PomodoroItemRepository;
import pl.gamilife.pomodoro.infrastructure.persistence.jpa.PomodoroTaskRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class PomodoroItemRepositoryImpl implements PomodoroItemRepository {

    private final PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa;

    public PomodoroItemRepositoryImpl(PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa) {
        this.pomodoroTaskRepositoryJpa = pomodoroTaskRepositoryJpa;
    }

    @Override
    public boolean existsByTaskId(UUID taskId) {
//        return pomodoroTaskRepositoryJpa.existsByTaskId(taskId);
        return false;
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
