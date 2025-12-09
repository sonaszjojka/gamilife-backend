package pl.gamilife.task.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.model.filter.TaskFilter;

import java.time.LocalDateTime;
import java.util.UUID;


@Component
public class TaskSpecificationBuilder {

    public Specification<Task> build(TaskFilter filter) {
        return Specification.allOf(
                selectedCategory(filter.categoryId()),
                selectedDifficulty(filter.difficultyId()),
                isGroupTask(filter.isGroupTask()),
                isCompleted(filter.isCompleted()),
                currentUser(filter.userId())
        );
    }

    private Specification<Task> isGroupTask(Boolean isGroupTask) {
        return ((root, query, criteriaBuilder) ->
        {
            if (isGroupTask == null) {
                return null;
            }
            return criteriaBuilder.equal(root.get("isGroupTask"), isGroupTask);
        }
        );
    }

    private Specification<Task> isCompleted(Boolean isCompleted) {
        return (root, query, cb) -> {
            if (isCompleted == null) return null;

            LocalDateTime now = LocalDateTime.now();

            Predicate endedByDate = cb.lessThan(root.get("deadline"), now);
            Predicate hasCompletedDate = cb.isNotNull(root.get("completedAt"));

            Predicate notEnded = cb.greaterThanOrEqualTo(root.get("deadline"), now);
            Predicate noCompletedDate = cb.isNull(root.get("completedAt"));

            if (isCompleted) {

                return cb.or(endedByDate, hasCompletedDate);
            } else {

                return cb.and(notEnded, noCompletedDate);
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
