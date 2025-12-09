package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

@Getter
@Entity
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(exclude = {"task"})
@Table(name = "habit")
public class Habit extends BaseEntity {

    @Column(name = "task_id", nullable = false, unique = true, updatable = false)
    private UUID taskId;

    @OneToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "task_id", nullable = false, insertable = false, updatable = false)
    private Task task;

    @Setter
    @Column(name = "cycle_length", nullable = false)
    private Duration cycleLength;

    @Column(name = "current_streak", nullable = false)
    private Integer currentStreak;

    @Column(name = "longest_streak", nullable = false)
    private Integer longestStreak;

    @Column(name = "finished_at")
    private Instant finishedAt;

    public void finish() {
        finishedAt = Instant.now();
    }
}
