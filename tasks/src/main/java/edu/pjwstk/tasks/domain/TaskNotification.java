package edu.pjwstk.tasks.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "task_notification")
public class TaskNotification {

    @Id
    @Column(name = "id", nullable = false, updatable = false)
    private Integer id;

    @Column(name = "send_date", nullable = false)
    private LocalDateTime sendDate;

    @ManyToOne
    @JoinColumn(name = "task_id")
    private Task task;
}
