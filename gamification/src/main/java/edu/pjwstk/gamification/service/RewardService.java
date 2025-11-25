package edu.pjwstk.gamification.service;

import java.util.UUID;

public interface RewardService {
    void rewardUser(UUID userId, int experience, int money);
}
