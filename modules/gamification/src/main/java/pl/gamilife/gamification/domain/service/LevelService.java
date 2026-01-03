package pl.gamilife.gamification.domain.service;

import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;

import java.util.List;
import java.util.Optional;

public interface LevelService {
    int levelUpUser(GamificationUser rewardedUser, List<Level> gainedLevels);

    List<Level> checkIfUserEligibleForLevelUp(int currentLevel, int newExperience);

    Optional<Level> getNextLevel(int currentLevel);
}
