package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Entity
@SuperBuilder
@NoArgsConstructor
@Table(name = "task")
@ToString(exclude = {"category", "difficulty", "taskNotifications"})
public class Task extends BaseEntity {

    @Setter
    @Column(name = "title", length = 200, nullable = false)
    private String title;

    @Setter
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

    @Setter
    @Column(name = "deadline")
    private Instant deadline;

    @Column(name = "completed_at")
    private Instant completedAt;

    @Builder.Default
    @Column(name = "reward_issued", nullable = false)
    private boolean rewardIssued = false;

    @OneToOne(mappedBy = "task")
    private Habit habit;

    @Builder.Default
    @OneToMany(mappedBy = "task")
    private Set<TaskNotification> taskNotifications = new HashSet<>();

    public boolean isGroupTask() {
        return userId == null;
    }

    public void setCategory(TaskCategory taskCategory) {
        this.category = taskCategory;
        this.categoryId = taskCategory.getId();
    }

    public void setDifficulty(TaskDifficulty taskDifficulty) {
        this.difficulty = taskDifficulty;
        this.difficultyId = taskDifficulty.getId();
    }

    public void complete() {
        this.completedAt = Instant.now();
    }
}
