package edu.pjwstk.core.event;

import lombok.Builder;
import lombok.Getter;

import java.util.UUID;

@Getter
@Builder
public class HabitStreakDownEvent {
    private UUID userId;
}
