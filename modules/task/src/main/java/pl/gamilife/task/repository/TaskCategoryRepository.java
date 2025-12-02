package pl.gamilife.task.repository;

import pl.gamilife.task.entity.TaskCategory;
import jakarta.validation.constraints.NotNull;

import java.util.Optional;

public interface TaskCategoryRepository {
    Optional<TaskCategory> findById(@NotNull(message = "Category Id cannot be null") Integer categoryId);
}
