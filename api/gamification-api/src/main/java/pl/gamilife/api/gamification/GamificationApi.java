package pl.gamilife.api.gamification;

import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;

import java.util.UUID;

public interface GamificationApi {
    void initUserStatisticsFor(UUID userId);

    StartingGamificationValuesDto getStartingGamificationValues();
}
