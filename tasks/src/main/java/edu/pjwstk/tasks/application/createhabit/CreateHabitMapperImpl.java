package edu.pjwstk.tasks.application.createhabit;

import edu.pjwstk.tasks.entity.Habit;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateHabitMapperImpl implements CreateHabitMapper {
    public Habit toEntity(CreateHabitRequest req, UUID habitId) {
        return Habit.builder()
                .id(habitId)
                .cycleLength(req.cycleLength())
                .currentStreak(req.currentStreak())
                .longestStreak(req.longestStreak())
                .isAccepted(req.isAccepted())
                .acceptedDate(req.acceptedDate())
                .declineMessage(req.declineMessage())
                .build();
    }

    public CreateHabitResponse toResponse(Habit habit) {
        return new CreateHabitResponse(
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
