package pl.gamilife.gamification.domain.service;

import pl.gamilife.gamification.domain.model.UserStatistic;

public interface AchievementService {
    void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic);
}
