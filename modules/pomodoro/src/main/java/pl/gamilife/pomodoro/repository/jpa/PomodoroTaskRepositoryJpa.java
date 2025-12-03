package pl.gamilife.pomodoro.repository.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;
import pl.gamilife.pomodoro.entity.PomodoroTask;

import java.util.UUID;

public interface PomodoroTaskRepositoryJpa extends JpaRepository<PomodoroTask, UUID> {

    boolean existsByTaskId(UUID taskId);

    PomodoroTaskDto findByTaskId(UUID taskId);
}
