package edu.pjwstk.tasks.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "task")
public class Task {

    @Id
    @Column(name = "task_id", updatable = false, nullable = false)
    private UUID id;

    @Column(name = "title", length = 200, nullable = false)
    private String title;

    @Column(name = "start_time", nullable = false)
    private LocalDateTime startTime;

    @Column(name = "end_time", nullable = true)
    private LocalDateTime endTime;

    @ManyToOne
    @JoinColumn(name = "category_id", nullable = false)
    private TaskCategory category;

    @ManyToOne
    @JoinColumn(name = "difficulty_id", nullable = false)
    private TaskDifficulty difficulty;

    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Column(name = "completed_at", nullable = true)
    private LocalDateTime completedAt;

    @ManyToOne
    @JoinColumn(name = "task_habit_id", nullable = true)
    private Habit habitTask;

    @OneToOne
    @JoinColumn(name = "previous_task_id", nullable = true)
    private Task previousTask;

    @OneToOne(mappedBy = "previousTask")
    private Task inversePreviousTask;

    @Column(name = "description", length = 200)
    private String description;

    @OneToMany(mappedBy = "task")
    @ToString.Exclude
    private List<TaskNotification> taskNotifications;

    //todo: pomodoro + group_task relations when they are done

}
