package edu.pjwstk.api.gamification;

import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.api.gamification.dto.StartingGamificationValuesDto;

import java.util.UUID;

public interface GamificationApi {
    void initUserStatisticsFor(UUID userId);

    StartingGamificationValuesDto getStartingGamificationValues();

    GetRequiredExperienceByLevelIdResult getRequiredExperienceByLevelId(Integer levelId);
}
