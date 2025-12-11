package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "task")
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

    @Column(name = "deadline")
    private Instant deadline;

    @Column(name = "completed_at")
    private Instant completedAt;

    @Column(name = "reward_issued", nullable = false)
    private boolean rewardIssued = false;

    @OneToMany(mappedBy = "task")
    private Set<TaskNotification> taskNotifications = new HashSet<>();

    private Task(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, Instant deadline) {
        setTitle(title);
        setDescription(description);
        setUserId(userId);
        setCategory(category);
        setDifficulty(difficulty);
        setDeadline(deadline);
    }

    public static Task create(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, Instant deadline) {
        if (userId == null) {
            throw new DomainValidationException("User id cannot be null");
        }

        return new Task(title, description, userId, category, difficulty, deadline);
    }

    public static Task createForGroupTask(String title, String description, TaskCategory category, TaskDifficulty difficulty, Instant deadline) {
        return new Task(title, description, null, category, difficulty, deadline);
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

    public void setDeadline(Instant deadline) {
        if (deadline == null) {
            throw new DomainValidationException("Deadline cannot be null");
        }

        if (!deadline.isAfter(Instant.now())) {
            throw new DomainValidationException("Deadline cannot be in the past");
        }

        this.deadline = deadline;
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

    public boolean complete() {
        if (this.completedAt != null) {
            throw new DomainValidationException("Task has already been completed");
        }

        this.completedAt = Instant.now();
        return !this.completedAt.isAfter(this.deadline);
    }
}
