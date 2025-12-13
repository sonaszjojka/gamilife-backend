package pl.gamilife.pomodoro.domain.model;


import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
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
    private Integer cyclesCompleted = 0;

    @Column(name = "task_id")
    private UUID taskId;

    @Column(name = "habit_id")
    private UUID habitId;

    private PomodoroItem(Integer cyclesRequired, UUID taskId, UUID habitId) {
        if (taskId == null && habitId == null) {
            throw new DomainValidationException("Either taskId or habitId must be provided");
        }

        if (taskId != null && habitId != null) {
            throw new DomainValidationException("Only one of taskId or habitId must be provided");
        }

        changeCyclesRequired(cyclesRequired);

        this.taskId = taskId;
        this.habitId = habitId;
    }

    public static PomodoroItem createForTask(Integer cyclesRequired, UUID taskId) {
        return new PomodoroItem(cyclesRequired, taskId, null);
    }

    public static PomodoroItem createForHabit(Integer cyclesRequired, UUID habitId) {
        return new PomodoroItem(cyclesRequired, null, habitId);
    }

    public void changeCyclesRequired(int cyclesRequired) {
        if (cyclesRequired <= 0) {
            throw new DomainValidationException("Cycles required cannot be negative or zero");
        }

        if (cyclesRequired < cyclesCompleted) {
            throw new DomainValidationException("Cycles required cannot be lower than the cycles already completed");
        }

        if (cyclesRequired == cyclesCompleted) {
            throw new DomainValidationException("Cannot complete the pomodoro item by lowering the number of cycles required");
        }

        this.cyclesRequired = cyclesRequired;
    }

    public boolean increaseCyclesCompleted(int amount) {
        if (amount < 0) {
            throw new DomainValidationException("Cannot decrease the number of cycles completed");
        }

        this.cyclesCompleted += amount;

        return this.cyclesCompleted >= this.cyclesRequired;
    }
}
