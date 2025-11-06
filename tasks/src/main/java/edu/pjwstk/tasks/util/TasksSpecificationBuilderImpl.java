package edu.pjwstk.tasks.util;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;


@Component
public class TasksSpecificationBuilderImpl implements TasksSpecificationBuilder{


    @Override
    public Specification<Task> build(
            Integer categoryId,
            Integer difficultyId,
            Boolean isGroupTask,
            Boolean isCompleted,
            UUID userId
    ) {
        return Specification.allOf(
                selectedCategory(categoryId),
                selectedDifficulty(difficultyId),
                isGroupTask(isGroupTask),
                isCompleted(isCompleted),
                currentUser(userId)
        );
    }



    @Override
    public Specification<Task> isGroupTask(Boolean isGroupTask) {
        return ((root, query, criteriaBuilder) ->
            {
                if (isGroupTask == null) {
                    return null;
                }
                return criteriaBuilder.equal(root.get("isGroupTask"), isGroupTask);
            }
        );
    }

    @Override
    public Specification<Task> isCompleted(Boolean isCompleted) {
        return (root, query, cb) -> {
            if (isCompleted == null) return null;

            LocalDateTime now = LocalDateTime.now();

            Predicate endedByDate = cb.lessThan(root.get("endTime"), now);
            Predicate hasCompletedDate = cb.isNotNull(root.get("completedAt"));

            Predicate notEnded = cb.greaterThanOrEqualTo(root.get("endTime"), now);
            Predicate noCompletedDate = cb.isNull(root.get("completedAt"));

            if (isCompleted) {

                return cb.or(endedByDate, hasCompletedDate);
            } else {

                return cb.and(notEnded, noCompletedDate);
            }
        };
    }



    @Override
    public Specification<Task> selectedDifficulty(Integer difficultyId) {
        return ((root, query, criteriaBuilder) ->
                    {
                        if (difficultyId == null) return null;
                        Join<Task, TaskDifficulty> difficultyJoin = root.join("difficulty");
                        return criteriaBuilder.equal(difficultyJoin.get("id"), difficultyId);
                    }

                );
    }

    @Override
    public Specification<Task> selectedCategory(Integer categoryId) {
        return ((root, query, criteriaBuilder) ->{

                        if (categoryId == null) return null;
            Join<Task, TaskCategory> categoryJoin = root.join("category");
            return criteriaBuilder.equal(categoryJoin.get("id"), categoryId);
                        }
        );
    }

    @Override
    public Specification<Task> currentUser(UUID userId) {
        return ((root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("userId"), userId)
        );
    }

    @Override
    public Specification<Task> hasHabit() {
        return null;
    }
}
