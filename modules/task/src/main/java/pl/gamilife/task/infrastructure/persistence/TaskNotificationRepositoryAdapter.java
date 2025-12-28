package pl.gamilife.task.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.task.domain.model.TaskNotification;
import pl.gamilife.task.domain.port.repository.TaskNotificationRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaTaskNotificationRepository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class TaskNotificationRepositoryAdapter implements TaskNotificationRepository {

    private final JpaTaskNotificationRepository repositoryJpa;

    public TaskNotificationRepositoryAdapter(JpaTaskNotificationRepository repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public TaskNotification save(TaskNotification taskNotification) {
        return repositoryJpa.save(taskNotification);
    }

    @Override
    public void deleteByIdAndTaskId(UUID taskId, UUID taskNotificationId) {
        repositoryJpa.deleteByTaskIdAndId(taskId, taskNotificationId);
    }

    @Override
    public Optional<TaskNotification> findByIdAndTaskId(UUID taskId, UUID taskNotificationId) {
        return repositoryJpa.findByTaskIdAndId(taskId, taskNotificationId);
    }
}
