package pl.gamilife.gamification.domain.service;

import pl.gamilife.gamification.domain.model.Level;

import java.util.List;
import java.util.UUID;

public interface LevelService {
    void levelUpUser(UUID userId, List<Level> gainedLevels);

    List<Level> checkIfUserEligibleForLevelUp(int currentLevel, int newExperience);
}
