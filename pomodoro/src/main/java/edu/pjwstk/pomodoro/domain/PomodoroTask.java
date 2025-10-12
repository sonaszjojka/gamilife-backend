package edu.pjwstk.pomodoro.domain;


import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "pomodoro_task")
public class PomodoroTask extends AbstractEntitySuperclass {

    @Id
    @Column(name = "pomodoro_id", nullable = false, updatable = false, unique = true)
    private UUID pomodoroId;

    @Column(name = "work_cycles_needed")
    private Integer workCyclesNeeded;

    @Column(name = "work_cycles_completed")
    private Integer workCyclesCompleted;

    @Column(name = "task_id")
    private UUID taskId;

}
