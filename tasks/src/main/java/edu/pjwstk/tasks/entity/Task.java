package edu.pjwstk.tasks.entity;

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
    @OneToOne
    @JoinColumn(name = "task_habit_id", nullable = true)
    private Habit habitTask;

    @Column(name = "user_id", nullable = true)
    private UUID userId;

    @Column(name = "completed_at", nullable = true)
    private LocalDateTime completedAt;


    @Column(name = "description", length = 200)
    private String description;

    @Column(name = "is_group_task", nullable = false,updatable = false)
    private Boolean isGroupTask;

    @OneToMany(mappedBy = "task", cascade = CascadeType.REMOVE, orphanRemoval = true)
    @ToString.Exclude
    private List<TaskNotification> taskNotifications;

    //tdo: pomodoro + group_task relations when they are done

}
