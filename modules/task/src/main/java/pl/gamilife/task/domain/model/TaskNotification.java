package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.*;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@ToString(exclude = {"task"})
@Table(name = "task_notification", schema = "task")
public class TaskNotification extends BaseEntity {

    @Setter
    @Column(name = "send_at", nullable = false)
    private Instant sendAt;

    @Column(name = "task_id", nullable = false, updatable = false)
    private UUID taskId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "task_id", nullable = false, insertable = false, updatable = false)
    private Task task;

    private TaskNotification(UUID taskId, Instant sendDate) {
        this.taskId = taskId;
        this.sendAt = sendDate;
    }

    public static TaskNotification create(UUID taskId, Instant sendAt) {
        return new TaskNotification(taskId, sendAt);
    }

}
