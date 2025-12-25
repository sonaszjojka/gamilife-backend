package pl.gamilife.task.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.model.filter.TaskFilter;

import java.util.UUID;


@Component
public class TaskSpecificationBuilder {

    public Specification<Task> build(TaskFilter filter) {
        return Specification.allOf(
                selectedCategory(filter.categoryId()),
                selectedDifficulty(filter.difficultyId()),
                isCompleted(filter.isCompleted()),
                currentUser(filter.userId())
        );
    }


    private Specification<Task> isCompleted(Boolean completed) {
        return (root, query, cb) -> {
            if (completed == null) {
                return null;
            }

            if (completed) {
                return cb.isNotNull(root.get("completedAt"));
            } else {
                return cb.isNull(root.get("completedAt"));
            }
        };
    }

    private Specification<Task> selectedDifficulty(Integer difficultyId) {
        return ((root, query, criteriaBuilder) ->
        {
            if (difficultyId == null) return null;
            Join<Task, TaskDifficulty> difficultyJoin = root.join("difficulty");
            return criteriaBuilder.equal(difficultyJoin.get("id"), difficultyId);
        }

        );
    }

    private Specification<Task> selectedCategory(Integer categoryId) {
        return ((root, query, criteriaBuilder) -> {

            if (categoryId == null) return null;
            Join<Task, TaskCategory> categoryJoin = root.join("category");
            return criteriaBuilder.equal(categoryJoin.get("id"), categoryId);
        }
        );
    }

    private Specification<Task> currentUser(UUID userId) {
        return ((root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("userId"), userId)
        );
    }
}
