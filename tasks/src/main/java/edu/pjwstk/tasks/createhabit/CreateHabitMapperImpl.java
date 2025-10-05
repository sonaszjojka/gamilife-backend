package edu.pjwstk.tasks.createhabit;

import edu.pjwstk.tasks.domain.Habit;
import jakarta.persistence.Column;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateHabitMapperImpl implements CreateHabitMapper{
    public Habit toEntity(CreateHabitRequest req, UUID habitId) {
        return Habit.builder()
                .id(habitId)
                .cycleLength(req.cycleLength())
                .currentStreak(req.currentStreak())
                .longestStreak(req.longestStreak())
                .createdAt(req.createdAt())
                .isAccepted(req.isAccepted())
                .acceptedDate(req.acceptedDate())
                .declineMessage(req.declineMessage())
                .lastEdit(req.lastEdit())
                .build();
    }

    public CreateHabitResponse toResponse(Habit habit) {
        return new CreateHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getCreatedAt(),
                habit.getIsAccepted(),
                habit.getAcceptedDate(),
                habit.getDeclineMessage(),
                habit.getLastEdit()
        );
    }
}
