package pl.gamilife.task.domain.port.repository;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.filter.TaskFilter;

import java.util.Optional;
import java.util.UUID;

public interface TaskRepository {
    Task save(Task task);

    Optional<Task> findById(UUID taskId);

    void deleteById(UUID taskId);

    Page<Task> findAllWithCategoryAndDifficulty(TaskFilter filter, Integer pageNumber, Integer pageSize);
}
