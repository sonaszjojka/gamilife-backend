package pl.gamilife.task.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.enums.ActivityType;
import pl.gamilife.task.domain.model.filter.ActivityItemFilter;
import pl.gamilife.task.domain.model.projection.ActivityItem;

import java.time.LocalDate;
import java.util.UUID;

@Component
public class ActivityItemSpecificationBuilder {

    public Specification<ActivityItem> build(ActivityItemFilter filter) {
        return Specification.allOf(
                matchesTitle(filter.title()),
                selectedCategory(filter.categoryId()),
                selectedDifficulty(filter.difficultyId()),
                currentUser(filter.userId()),
                deadlineBetween(filter.startDate(), filter.endDate()),
                aliveIfHabit(filter.currentUserDate())
        );
    }

    private Specification<ActivityItem> deadlineBetween(LocalDate start, LocalDate end) {
        return (root, query, criteriaBuilder) -> {
            if (start == null && end == null) {
                return null;
            }

            Predicate startPredicate = start != null
                    ? criteriaBuilder.greaterThanOrEqualTo(root.get("deadlineDate"), start)
                    : null;

            Predicate endPredicate = end != null
                    ? criteriaBuilder.lessThanOrEqualTo(root.get("deadlineDate"), end)
                    : null;

            if (startPredicate != null && endPredicate != null) {
                return criteriaBuilder.and(startPredicate, endPredicate);
            } else if (startPredicate != null) {
                return startPredicate;
            } else {
                return endPredicate;
            }
        };
    }

    private Specification<ActivityItem> matchesTitle(String title) {
        return (root, query, cb) -> {
            if (title == null || title.isBlank()) {
                return null;
            }
            String searchPattern = "%" + title.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("title")), searchPattern);
        };
    }

    private Specification<ActivityItem> selectedDifficulty(Integer difficultyId) {
        return (root, query, criteriaBuilder) -> {
            if (difficultyId == null) {
                return null;
            }

            return criteriaBuilder.equal(root.get("difficultyId"), difficultyId);
        };
    }

    private Specification<ActivityItem> selectedCategory(Integer categoryId) {
        return (root, query, criteriaBuilder) -> {
            if (categoryId == null) {
                return null;
            }

            return criteriaBuilder.equal(root.get("categoryId"), categoryId);
        };
    }

    private Specification<ActivityItem> currentUser(UUID userId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("userId"), userId);
    }

    private Specification<ActivityItem> aliveIfHabit(LocalDate currentUserDate) {
        return (root, query, cb) -> {
            if (currentUserDate == null) {
                return null;
            }

            Predicate isTask = cb.equal(root.get("type"), ActivityType.TASK);
            Predicate isAliveHabit = cb.and(
                    cb.equal(root.get("type"), ActivityType.HABIT),
                    cb.greaterThanOrEqualTo(root.get("deadlineDate"), currentUserDate)
            );

            return cb.or(isTask, isAliveHabit);
        };
    }
}
