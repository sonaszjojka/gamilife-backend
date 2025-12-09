package pl.gamilife.task.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.entity.TaskNotification;

import java.util.Optional;
import java.util.UUID;

public interface TaskNotificationRepositoryJpa extends JpaRepository<TaskNotification, UUID> {
    TaskNotification task(Task task);

    Optional<TaskNotification> findByTaskIdAndId(UUID taskId, UUID id);

    void deleteByTaskIdAndId(UUID taskId, UUID id);
}
