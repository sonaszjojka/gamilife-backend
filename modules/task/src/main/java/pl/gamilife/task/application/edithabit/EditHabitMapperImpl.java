package pl.gamilife.task.application.edithabit;

import org.springframework.stereotype.Component;
import pl.gamilife.task.entity.Habit;

@Component
public class EditHabitMapperImpl implements EditHabitMapper {

    public EditHabitResponse toResponse(Habit habit) {
        return new EditHabitResponse(
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
