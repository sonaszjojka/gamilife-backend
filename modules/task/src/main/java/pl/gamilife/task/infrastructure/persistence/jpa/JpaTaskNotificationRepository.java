package pl.gamilife.task.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskNotification;

import java.util.Optional;
import java.util.UUID;

public interface JpaTaskNotificationRepository extends JpaRepository<TaskNotification, UUID> {
    TaskNotification task(Task task);

    Optional<TaskNotification> findByTaskIdAndId(UUID taskId, UUID id);

    void deleteByTaskIdAndId(UUID taskId, UUID id);
}
