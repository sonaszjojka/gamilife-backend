package pl.gamilife.pomodoro.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.pomodoro.domain.PomodoroItem;

import java.util.UUID;

public interface PomodoroTaskRepositoryJpa extends JpaRepository<PomodoroItem, UUID> {

//    boolean existsByTaskId(UUID taskId);
//
//    PomodoroTaskDto findByTaskId(UUID taskId);
}
