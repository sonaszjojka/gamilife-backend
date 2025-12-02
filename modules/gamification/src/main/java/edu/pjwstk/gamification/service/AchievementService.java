package edu.pjwstk.gamification.service;

import edu.pjwstk.gamification.model.UserStatistic;

public interface AchievementService {

    void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic);

}
