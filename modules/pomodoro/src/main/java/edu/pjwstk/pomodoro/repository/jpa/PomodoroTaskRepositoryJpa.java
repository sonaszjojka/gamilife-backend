package edu.pjwstk.pomodoro.repository.jpa;


import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import edu.pjwstk.pomodoro.entity.PomodoroTask;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface PomodoroTaskRepositoryJpa extends JpaRepository<PomodoroTask, UUID> {

    boolean existsByTaskId(UUID taskId);

    PomodoroTaskDto findByTaskId(UUID taskId);
}
