package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskNotification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TaskNotificationRepositoryJpa extends JpaRepository<TaskNotification, Integer> {
    TaskNotification task(Task task);

    Optional<TaskNotification> findByTaskIdAndId(UUID taskId, Integer id);

    void deleteByTaskIdAndId(UUID taskId, Integer id);
}
