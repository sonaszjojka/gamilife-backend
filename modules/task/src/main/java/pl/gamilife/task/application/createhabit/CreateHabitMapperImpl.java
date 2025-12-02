package pl.gamilife.task.application.createhabit;

import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateHabitMapperImpl implements CreateHabitMapper {
    public Habit toEntity(CreateHabitRequest req, UUID habitId, Task habitTask) {
        return Habit.builder()
                .id(habitId)
                .task(habitTask)
                .cycleLength(req.cycleLength())
                .currentStreak(req.currentStreak())
                .longestStreak(req.longestStreak())
                .acceptedDate(req.acceptedDate())
                .build();
    }

    public CreateHabitResponse toResponse(Habit habit) {
        return new CreateHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getAcceptedDate(),
                habit.getUpdatedAt(),
                habit.getCreatedAt()
        );
    }
}
