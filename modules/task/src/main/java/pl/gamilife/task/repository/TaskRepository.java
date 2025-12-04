package pl.gamilife.task.repository;

import pl.gamilife.task.entity.Task;

import java.util.Optional;
import java.util.UUID;

public interface TaskRepository {
    Task save(Task task);

    Optional<Task> findById(UUID taskId);

    void deleteById(UUID taskId);

    Boolean existsById(UUID taskId);

}
