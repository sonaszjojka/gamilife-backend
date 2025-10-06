package edu.pjwstk.tasks.edithabit;

import edu.pjwstk.tasks.createhabit.CreateHabitResponse;
import edu.pjwstk.tasks.domain.Habit;
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
