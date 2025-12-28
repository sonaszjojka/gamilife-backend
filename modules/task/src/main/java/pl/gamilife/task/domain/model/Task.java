package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;
import pl.gamilife.task.domain.model.enums.TaskStatus;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "task", schema = "task")
@ToString(exclude = {"category", "difficulty", "taskNotifications"})
public class Task extends BaseEntity {

    @Column(name = "title", length = 200, nullable = false)
    private String title;

    @Column(name = "description", length = 500)
    private String description;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "category_id", nullable = false)
    private Integer categoryId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "category_id", nullable = false, insertable = false, updatable = false)
    private TaskCategory category;

    @Column(name = "difficulty_id", nullable = false)
    private Integer difficultyId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "difficulty_id", nullable = false, insertable = false, updatable = false)
    private TaskDifficulty difficulty;

    @Column(name = "deadline_date", nullable = false)
    private LocalDate deadlineDate;

    @Column(name = "deadline_time")
    private LocalTime deadlineTime;

    @Column(name = "completed_at")
    private Instant completedAt;

    @Column(name = "reward_issued", nullable = false)
    private boolean rewardIssued = false;

    @OneToMany(mappedBy = "task")
    private final Set<TaskNotification> taskNotifications = new HashSet<>();

    private Task(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, LocalDate deadlineDate, LocalTime deadlineTime, LocalDateTime currentUserDateTime) {
        setTitle(title);
        setDescription(description);
        setUserId(userId);
        setCategory(category);
        setDifficulty(difficulty);
        rescheduleDeadline(deadlineDate, deadlineTime, currentUserDateTime);
    }

    public static Task createPrivate(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, LocalDate deadlineDate, LocalTime deadlineTime, LocalDateTime currentUserDateTime) {
        if (userId == null) {
            throw new DomainValidationException("User id cannot be null");
        }

        return new Task(title, description, userId, category, difficulty, deadlineDate, deadlineTime, currentUserDateTime);
    }

    public static Task createForGroupTask(String title, String description, TaskCategory category, TaskDifficulty difficulty, LocalDate deadlineDate, LocalTime deadlineTime, LocalDateTime currentGroupDateTime) {
        return new Task(title, description, null, category, difficulty, deadlineDate, deadlineTime, currentGroupDateTime);
    }

    public boolean isOwnedBy(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User id cannot be null");
        }

        if (isGroupTask()) {
            return false;
        }

        return this.userId.equals(userId);
    }

    public void setTitle(String title) {
        if (title == null || title.isBlank()) {
            throw new DomainValidationException("Title cannot be null or empty");
        }

        if (title.length() > 200) {
            throw new DomainValidationException("Title cannot be longer than 200 characters");
        }

        this.title = title;
    }

    public void setDescription(String description) {
        if (description == null || description.isBlank()) {
            this.description = null;
            return;
        }

        if (description.length() > 500) {
            throw new DomainValidationException("Description cannot be longer than 500 characters");
        }

        this.description = description;
    }

    private void setUserId(UUID userId) {
        this.userId = userId;
    }

    public void rescheduleDeadline(LocalDate deadlineDate, LocalTime deadlineTime, LocalDateTime currentUserDateTime) {
        if (deadlineDate == null) {
            throw new DomainValidationException("Deadline cannot be null");
        }

        if (deadlineDate.isBefore(currentUserDateTime.toLocalDate())) {
            throw new DomainValidationException("Deadline cannot be in the past");
        }

        if (deadlineTime != null) {
            LocalDateTime deadlineDateTime = LocalDateTime.of(deadlineDate, deadlineTime);
            if (deadlineDateTime.isBefore(currentUserDateTime)) {
                throw new DomainValidationException("Deadline cannot be in the past");
            }
        }

        this.deadlineDate = deadlineDate;
        this.deadlineTime = deadlineTime;
    }

    public void setCategory(TaskCategory taskCategory) {
        if (taskCategory == null) {
            throw new DomainValidationException("Category cannot be null");
        }

        this.category = taskCategory;
        this.categoryId = taskCategory.getId();
    }

    public void setDifficulty(TaskDifficulty taskDifficulty) {
        if (taskDifficulty == null) {
            throw new DomainValidationException("Difficulty cannot be null");
        }

        this.difficulty = taskDifficulty;
        this.difficultyId = taskDifficulty.getId();
    }

    public boolean isGroupTask() {
        return userId == null;
    }

    public void markDone() {
        if (this.completedAt != null) {
            throw new DomainValidationException("Task has already been completed");
        }

        this.completedAt = Instant.now();
    }

    public void markUndone() {
        if (this.completedAt == null) {
            throw new DomainValidationException("Task is not marked as completed");
        }

        this.completedAt = null;
    }

    public void markRewardAsIssued() {
        rewardIssued = true;
    }

    public TaskStatus calculateCurrentStatus(LocalDateTime currentDateTime) {
        if (completedAt != null) {
            return TaskStatus.COMPLETED;
        }

        LocalDate currentDate = currentDateTime.toLocalDate();
        if (isWholeDayTask()) {
            if (currentDate.isAfter(deadlineDate)) {
                return TaskStatus.DEADLINE_MISSED;
            }

            if (currentDate.equals(deadlineDate)) {
                return TaskStatus.DEADLINE_TODAY;
            }

            return TaskStatus.INCOMPLETE;
        }

        LocalDateTime deadlineDateTime = getDeadlineDateTime();

        if (!currentDateTime.isBefore(deadlineDateTime)) {
            return TaskStatus.DEADLINE_MISSED;
        }

        if (currentDate.equals(deadlineDate)) {
            return TaskStatus.DEADLINE_TODAY;
        }

        return TaskStatus.INCOMPLETE;
    }

    private LocalDateTime getDeadlineDateTime() {
        return LocalDateTime.of(deadlineDate, deadlineTime);
    }

    public boolean isWholeDayTask() {
        return deadlineTime == null;
    }
}
