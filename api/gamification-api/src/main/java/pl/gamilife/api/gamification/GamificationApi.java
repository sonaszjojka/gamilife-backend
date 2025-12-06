package pl.gamilife.api.gamification;

import pl.gamilife.api.gamification.dto.GamificationValuesDto;

import java.util.UUID;

public interface GamificationApi {
    void initUserStatisticsFor(UUID userId);

    GamificationValuesDto getStartingGamificationValues();

    GamificationValuesDto getGamificationValuesForCompletedOnboarding();
}
