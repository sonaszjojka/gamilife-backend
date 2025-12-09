package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.UUID;

@Getter
@Entity
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(exclude = {"task"})
@Table(name = "task_notification")
public class TaskNotification extends BaseEntity {

    @Setter
    @Column(name = "send_date", nullable = false)
    private Instant sendDate;

    @Column(name = "task_id", nullable = false, updatable = false)
    private UUID taskId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "task_id", nullable = false, insertable = false, updatable = false)
    private Task task;

}
