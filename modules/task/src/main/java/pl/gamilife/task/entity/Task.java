package pl.gamilife.task.entity;

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

    @Column(name = "end_time")
    private LocalDateTime endTime;

    @ManyToOne
    @JoinColumn(name = "category_id", nullable = false)
    private TaskCategory category;

    @ManyToOne
    @JoinColumn(name = "difficulty_id", nullable = false)
    private TaskDifficulty difficulty;


    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "completed_at")
    private LocalDateTime completedAt;


    @Column(name = "description", length = 200)
    private String description;

    @Column(name = "is_group_task", nullable = false, updatable = false)
    private Boolean isGroupTask;

    @Column(name = "reward_issued", nullable = false)
    @Builder.Default
    private boolean rewardIssued = false;

    @OneToMany(mappedBy = "task", cascade = CascadeType.REMOVE, orphanRemoval = true)
    @ToString.Exclude
    private List<TaskNotification> taskNotifications;

    //tdo: pomodoro + group_task relations when they are done

}
