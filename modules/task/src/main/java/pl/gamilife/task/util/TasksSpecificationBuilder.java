package pl.gamilife.task.util;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.task.entity.Task;

import java.util.UUID;

public interface TasksSpecificationBuilder {

    Specification<Task> build(
            Integer categoryId,
            Integer difficultyId,
            Boolean isGroupTask,
            Boolean isCompleted,
            UUID userId
    );

    Specification<Task> isGroupTask(Boolean isGroupTask);

    Specification<Task> isCompleted(Boolean isCompleted);

    Specification<Task> hasHabit();

    Specification<Task> selectedDifficulty(Integer difficultyId);

    Specification<Task> selectedCategory(Integer categoryId);

    Specification<Task> currentUser(UUID userId);
}
