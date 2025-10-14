package edu.pjwstk.pomodoro.repository.jpa;

import edu.pjwstk.pomodoro.entity.PomodoroTask;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface PomodoroTaskRepositoryJpa extends JpaRepository<PomodoroTask, UUID> {

    boolean existsByTaskId(UUID taskId);
}
