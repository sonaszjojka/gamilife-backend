package edu.pjwstk.tasks.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "habit")
public class Habit extends AbstractEntitySuperclass {

    @Id
    @Column(name = "habit_id", nullable = false, updatable = false)
    private UUID id;

    @OneToOne
    @JoinColumn(name = "task_id", nullable = false, updatable = false)
    private Task task;

    @Column(name = "cycle_length", nullable = false)
    private Duration cycleLength;

    @Column(name = "current_streak", nullable = false)
    private Integer currentStreak;

    @Column(name = "longest_streak", nullable = false)
    private Integer longestStreak;

    @Column(name = "accepted_date", nullable = true)
    private LocalDateTime acceptedDate;

}
