package pl.gamilife.pomodoro.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.pomodoro.domain.PomodoroItem;
import pl.gamilife.pomodoro.domain.repository.PomodoroTaskRepository;
import pl.gamilife.pomodoro.infrastructure.persistence.jpa.PomodoroTaskRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class PomodoroTaskRepositoryImpl implements PomodoroTaskRepository {

    private final PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa;

    public PomodoroTaskRepositoryImpl(PomodoroTaskRepositoryJpa pomodoroTaskRepositoryJpa) {
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
    public void deleteByPomodoroTaskId(UUID pomodoroTaskId) {
        pomodoroTaskRepositoryJpa.deleteById(pomodoroTaskId);
    }

    @Override
    public Optional<PomodoroItem> findById(UUID pomodoroTaskId) {
        return pomodoroTaskRepositoryJpa.findById(pomodoroTaskId);
    }
}
