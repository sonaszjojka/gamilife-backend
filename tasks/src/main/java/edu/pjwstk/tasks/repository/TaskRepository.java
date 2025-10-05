package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.domain.Task;

import java.util.Optional;
import java.util.UUID;

public interface TaskRepository {
    Task save(Task task);

    Optional<Task> findById(UUID taskId);
}
