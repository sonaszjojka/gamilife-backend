package pl.gamilife.gamification.service;

import pl.gamilife.gamification.model.UserStatistic;

public interface AchievementService {

    void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic);

}
