package edu.pjwstk.pomodoro.repository.impl;

import edu.pjwstk.pomodoro.domain.PomodoroTask;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import edu.pjwstk.pomodoro.repository.jpa.PomodoroTaskRepositoryJpa;
import org.springframework.stereotype.Repository;

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
    public boolean existsByPomodoroTaskId(UUID pomodoroTaskId) {
        return pomodoroTaskRepositoryJpa.existsById(pomodoroTaskId);
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
