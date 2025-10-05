package edu.pjwstk.tasks.repository;

import edu.pjwstk.tasks.domain.Task;

public interface TaskRepository {
    Task save(Task task);
}
