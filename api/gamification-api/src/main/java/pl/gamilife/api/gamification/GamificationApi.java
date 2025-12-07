package pl.gamilife.api.gamification;

import java.util.UUID;

public interface GamificationApi {
    void initUserStatisticsFor(UUID userId);
}
