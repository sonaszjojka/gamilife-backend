package pl.gamilife.task.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.model.filter.HabitFilter;

import java.time.LocalDate;
import java.util.UUID;


@Component
public class HabitSpecificationBuilder {

    public Specification<Habit> build(HabitFilter filter) {
        return Specification.allOf(
                matchesTitle(filter.title()),
                selectedCategory(filter.categoryId()),
                selectedDifficulty(filter.difficultyId()),
                isAlive(filter.isAlive(), filter.currentUserDate()),
                currentUser(filter.userId())
        );
    }

    private Specification<Habit> matchesTitle(String title) {
        return (root, query, cb) -> {
            if (title == null || title.isBlank()) {
                return null;
            }
            String searchPattern = "%" + title.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("title")), searchPattern);
        };
    }

    private Specification<Habit> isAlive(boolean isAlive, LocalDate currentUserDate) {
        return (root, query, criteriaBuilder) -> {
            if (currentUserDate == null) {
                return null;
            }

            if (isAlive) {
                return criteriaBuilder.greaterThanOrEqualTo(root.get("currentDeadline"), currentUserDate);
            } else {
                return criteriaBuilder.lessThan(root.get("currentDeadline"), currentUserDate);
            }
        };
    }

    private Specification<Habit> selectedDifficulty(Integer difficultyId) {
        return (root, query, criteriaBuilder) -> {
            if (difficultyId == null) {
                return null;
            }

            Join<Habit, TaskDifficulty> difficultyJoin = root.join("difficulty");

            return criteriaBuilder.equal(difficultyJoin.get("id"), difficultyId);
        };
    }

    private Specification<Habit> selectedCategory(Integer categoryId) {
        return (root, query, criteriaBuilder) -> {
            if (categoryId == null) {
                return null;
            }

            Join<Habit, TaskCategory> categoryJoin = root.join("category");

            return criteriaBuilder.equal(categoryJoin.get("id"), categoryId);
        };
    }

    private Specification<Habit> currentUser(UUID userId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("userId"), userId);
    }
}
