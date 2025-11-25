package edu.pjwstk.gamification.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

@Getter
@AllArgsConstructor
public class RewardEarnedEvent {
    private UUID userId;
    private int experience;
    private int money;
}
