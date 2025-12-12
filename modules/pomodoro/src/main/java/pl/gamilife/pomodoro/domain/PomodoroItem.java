package pl.gamilife.pomodoro.domain;


import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@ToString
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "pomodoro_item")
public class PomodoroItem extends BaseEntity {

    @Column(name = "cycles_required", nullable = false)
    private Integer cyclesRequired;

    @Column(name = "cycles_completed", nullable = false)
    private Integer cyclesCompleted;

    @Column(name = "task_id")
    private UUID taskId;

    @Column(name = "habit_id")
    private UUID habitId;

}
