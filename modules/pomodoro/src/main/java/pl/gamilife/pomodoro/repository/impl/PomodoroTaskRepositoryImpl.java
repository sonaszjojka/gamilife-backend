package pl.gamilife.pomodoro.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.pomodoro.entity.PomodoroTask;
import pl.gamilife.pomodoro.repository.PomodoroTaskRepository;
import pl.gamilife.pomodoro.repository.jpa.PomodoroTaskRepositoryJpa;

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
        return pomodoroTaskRepositoryJpa.existsByTaskId(taskId);
    }

    @Override
    public PomodoroTask save(PomodoroTask pomodoroTask) {
        return pomodoroTaskRepositoryJpa.save(pomodoroTask);
    }

    @Override
    public void deleteByPomodoroTaskId(UUID pomodoroTaskId) {
        pomodoroTaskRepositoryJpa.deleteById(pomodoroTaskId);
    }

    @Override
    public Optional<PomodoroTask> findByPomodoroTaskId(UUID pomodoroTaskId) {
        return pomodoroTaskRepositoryJpa.findById(pomodoroTaskId);
    }
}
