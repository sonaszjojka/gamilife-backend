package edu.pjwstk.tasks.application.edithabit;

import edu.pjwstk.tasks.entity.Habit;
import org.springframework.stereotype.Component;

@Component
public class EditHabitMapperImpl implements EditHabitMapper {

    public EditHabitResponse toResponse(Habit habit) {
        return new EditHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getIsAccepted(),
                habit.getAcceptedDate(),
                habit.getDeclineMessage(),
                habit.getUpdatedAt(),
                habit.getCreatedAt()
        );
    }

}
