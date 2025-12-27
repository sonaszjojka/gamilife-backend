package pl.gamilife.app.persistence.specification;

import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.app.dto.activity.ActivityItemFilter;
import pl.gamilife.app.persistence.view.ActivityItem;
import pl.gamilife.shared.kernel.enums.ActivityType;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

@Component
public class ActivityItemSpecificationBuilder {

    private static final String DEADLINE_DATE = "deadlineDate";

    public Specification<ActivityItem> build(ActivityItemFilter filter) {
        return Specification.allOf(
                matchesTitle(filter.title()),
                currentUser(filter.userId()),
                selectedCategory(filter.categoryId()),
                selectedDifficulty(filter.difficultyId()),
                deadlineBetween(filter.startDate(), filter.endDate()),
                isWorkable(filter.workable(), filter.zoneId()),
                hasPomodoroItem(filter.pomodoro()),
                onlyActive(filter.zoneId())
        );
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

    private Specification<ActivityItem> currentUser(UUID userId) {
        return (root, query, cb) ->
                cb.equal(root.get("userId"), userId);
    }

    private Specification<ActivityItem> deadlineBetween(LocalDate start, LocalDate end) {
        return (root, query, criteriaBuilder) -> {
            if (start == null && end == null) {
                return null;
            }

            Predicate startPredicate = start != null
                    ? criteriaBuilder.greaterThanOrEqualTo(root.get(DEADLINE_DATE), start)
                    : null;

            Predicate endPredicate = end != null
                    ? criteriaBuilder.lessThanOrEqualTo(root.get(DEADLINE_DATE), end)
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

    private Specification<ActivityItem> hasPomodoroItem(Boolean pomodoro) {
        return (root, query, cb) -> {
            if (pomodoro == null) {
                return null;
            }

            if (pomodoro) {
                return cb.isNotNull(root.get("pomodoroId"));
            } else {
                return cb.isNull(root.get("pomodoroId"));
            }
        };
    }

    private Specification<ActivityItem> isWorkable(Boolean workable, ZoneId zoneId) {
        return (root, query, cb) -> {
            if (workable == null) {
                return null;
            }

            LocalDate currentUserDate = LocalDate.now(zoneId);
            Predicate isHabitWorkable = cb.greaterThan(cb.literal(currentUserDate), root.get("previousDeadlineDate"));
            if (workable) {
                return cb.or(
                        cb.equal(root.get("type"), pl.gamilife.shared.kernel.enums.ActivityType.TASK),
                        cb.and(
                                cb.equal(root.get("type"), pl.gamilife.shared.kernel.enums.ActivityType.HABIT),
                                isHabitWorkable
                        )
                );
            } else {
                return cb.and(
                        cb.equal(root.get("type"), pl.gamilife.shared.kernel.enums.ActivityType.HABIT),
                        cb.not(isHabitWorkable)
                );
            }
        };
    }

    private Specification<ActivityItem> onlyActive(ZoneId zoneId) {
        return (root, query, cb) -> {
            if (zoneId == null) {
                return null;
            }

            LocalDate currentUserDate = LocalDate.now(zoneId);
            Predicate isNotCompletedTask = cb.and(
                    cb.equal(root.get("type"), ActivityType.TASK),
                    cb.isNull(root.get("completedAt"))
            );
            Predicate isAliveHabit = cb.and(
                    cb.equal(root.get("type"), ActivityType.HABIT),
                    cb.greaterThanOrEqualTo(root.get(DEADLINE_DATE), currentUserDate)
            );

            return cb.or(isNotCompletedTask, isAliveHabit);
        };
    }

}
