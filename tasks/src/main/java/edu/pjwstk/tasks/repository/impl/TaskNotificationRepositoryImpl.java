package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.domain.TaskNotification;
import edu.pjwstk.tasks.repository.TaskNotificationRepository;
import edu.pjwstk.tasks.repository.jpa.TaskNotificationRepositoryJpa;
import org.springframework.stereotype.Repository;

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
}
