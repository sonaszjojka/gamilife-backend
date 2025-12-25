package pl.gamilife.app.persistence.specification;

import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.app.dto.ActivityItemWithPomodoroFilter;
import pl.gamilife.app.persistence.view.ActivityItemWithPomodoro;
import pl.gamilife.shared.kernel.enums.ActivityType;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

@Component
public class ActivityItemWithPomodoroSpecificationBuilder {

    public Specification<ActivityItemWithPomodoro> build(ActivityItemWithPomodoroFilter filter) {
        return Specification.allOf(
                matchesTitle(filter.title()),
                currentUser(filter.userId()),
                isWorkable(filter.workable(), filter.zoneId()),
                hasPomodoroItem(filter.pomodoro()),
                aliveIfHabit(filter.zoneId())
        );
    }

    private Specification<ActivityItemWithPomodoro> matchesTitle(String title) {
        return (root, query, cb) -> {
            if (title == null || title.isBlank()) {
                return null;
            }

            String searchPattern = "%" + title.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("title")), searchPattern);
        };
    }

    private Specification<ActivityItemWithPomodoro> currentUser(UUID userId) {
        return (root, query, cb) ->
                cb.equal(root.get("userId"), userId);
    }

    private Specification<ActivityItemWithPomodoro> hasPomodoroItem(Boolean pomodoro) {
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

    private Specification<ActivityItemWithPomodoro> isWorkable(Boolean workable, ZoneId zoneId) {
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

    private Specification<ActivityItemWithPomodoro> aliveIfHabit(ZoneId zoneId) {
        return (root, query, cb) -> {
            if (zoneId == null) {
                return null;
            }

            LocalDate currentUserDate = LocalDate.now(zoneId);
            Predicate isTask = cb.equal(root.get("type"), ActivityType.TASK);
            Predicate isAliveHabit = cb.and(
                    cb.equal(root.get("type"), ActivityType.HABIT),
                    cb.greaterThanOrEqualTo(root.get("deadlineDate"), currentUserDate)
            );

            return cb.or(isTask, isAliveHabit);
        };
    }

}
