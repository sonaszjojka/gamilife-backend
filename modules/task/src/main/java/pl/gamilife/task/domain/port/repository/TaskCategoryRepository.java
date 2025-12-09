package pl.gamilife.task.domain.port.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.task.domain.model.TaskCategory;

import java.util.Optional;

public interface TaskCategoryRepository {
    Optional<TaskCategory> findById(@NotNull(message = "Category Id cannot be null") Integer categoryId);
}
