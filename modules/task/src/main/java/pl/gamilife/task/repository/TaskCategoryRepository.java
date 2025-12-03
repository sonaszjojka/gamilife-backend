package pl.gamilife.task.repository;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.task.entity.TaskCategory;

import java.util.Optional;

public interface TaskCategoryRepository {
    Optional<TaskCategory> findById(@NotNull(message = "Category Id cannot be null") Integer categoryId);
}
