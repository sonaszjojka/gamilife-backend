package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.entity.TaskNotification;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import edu.pjwstk.tasks.repository.jpa.TaskNotificationRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class TaskNotificationRepositoryImpl implements TaskNotificationRepository {

    private final TaskNotificationRepositoryJpa repositoryJpa;

    public TaskNotificationRepositoryImpl(TaskNotificationRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public TaskNotification save(TaskNotification taskNotification) {
        return repositoryJpa.save(taskNotification);
    }

    @Override
    public void deleteByIdAndTaskId(UUID taskId, Integer taskNotificationId) {
        repositoryJpa.deleteByTaskIdAndId(taskId, taskNotificationId);
    }

    @Override
    public Optional<TaskNotification> findByIdAndTaskId(UUID taskId, Integer taskNotificationId) {
        return repositoryJpa.findByTaskIdAndId(taskId, taskNotificationId);
    }
}
