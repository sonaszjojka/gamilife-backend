package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.domain.TaskCategory;
import edu.pjwstk.tasks.domain.TaskNotification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskNotificationRepositoryJpa extends JpaRepository<TaskNotification, Integer> {
}
