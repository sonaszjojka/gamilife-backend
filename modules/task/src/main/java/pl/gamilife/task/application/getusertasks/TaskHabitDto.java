package pl.gamilife.task.application.getusertasks;

import lombok.Builder;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record TaskHabitDto(

        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        LocalDateTime acceptedDate
) {


}
