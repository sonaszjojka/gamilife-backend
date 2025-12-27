package pl.gamilife.app.persistence.view;

import jakarta.persistence.*;
import jakarta.validation.constraints.Size;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Immutable;
import pl.gamilife.app.enums.ActivityStatus;
import pl.gamilife.shared.kernel.enums.ActivityType;
import pl.gamilife.shared.persistence.entity.BaseUuidReadOnlyEntity;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "v_activity_item")
public class ActivityItem extends BaseUuidReadOnlyEntity {

    @Column(name = "type", length = Integer.MAX_VALUE)
    @Enumerated(EnumType.STRING)
    private ActivityType type;

    @Size(max = 200)
    @Column(name = "title", length = 200)
    private String title;

    @Size(max = 500)
    @Column(name = "description", length = 500)
    private String description;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "category_id")
    private Integer categoryId;

    @Size(max = 50)
    @Column(name = "category_name", length = 50)
    private String categoryName;

    @Column(name = "difficulty_id")
    private Integer difficultyId;

    @Size(max = 50)
    @Column(name = "difficulty_name", length = 50)
    private String difficultyName;

    @Column(name = "deadline_date")
    private LocalDate deadlineDate;

    @Column(name = "deadline_time")
    private LocalTime deadlineTime;

    @Column(name = "cycle_length")
    private Integer cycleLength;

    @Column(name = "current_streak")
    private Integer currentStreak;

    @Column(name = "longest_streak")
    private Integer longestStreak;

    @Column(name = "previous_deadline_date")
    private LocalDate previousDeadlineDate;

    @Column(name = "pomodoro_id")
    private UUID pomodoroId;

    @Column(name = "cycles_completed")
    private Integer cyclesCompleted;

    @Column(name = "cycles_required")
    private Integer cyclesRequired;

    public ActivityStatus calculateCurrentStatus(LocalDateTime currentUserDateTime) {
        if (type == ActivityType.HABIT) {
            return ActivityStatus.ALIVE;
        }

        LocalDate currentUserDate = currentUserDateTime.toLocalDate();
        if (isWholeDayTask()) {
            if (currentUserDate.isAfter(deadlineDate)) {
                return ActivityStatus.DEADLINE_MISSED;
            }

            if (currentUserDate.equals(deadlineDate)) {
                return ActivityStatus.DEADLINE_TODAY;
            }

            return ActivityStatus.INCOMPLETE;
        }

        LocalDateTime deadlineDateTime = getDeadlineDateTime();

        if (!currentUserDateTime.isBefore(deadlineDateTime)) {
            return ActivityStatus.DEADLINE_MISSED;
        }

        if (currentUserDate.equals(deadlineDate)) {
            return ActivityStatus.DEADLINE_TODAY;
        }

        return ActivityStatus.INCOMPLETE;
    }

    private LocalDateTime getDeadlineDateTime() {
        if (type == ActivityType.HABIT) {
            throw new UnsupportedOperationException("This method works only for tasks");
        }

        return LocalDateTime.of(deadlineDate, deadlineTime);
    }

    private boolean isWholeDayTask() {
        if (type == ActivityType.HABIT) {
            throw new UnsupportedOperationException("This method works only for tasks");
        }

        return deadlineTime == null;
    }

    public boolean canBeWorkedOn(LocalDate currentUserDate) {
        if (type == ActivityType.TASK) {
            return true;
        }

        return currentUserDate.isAfter(previousDeadlineDate);
    }
}
