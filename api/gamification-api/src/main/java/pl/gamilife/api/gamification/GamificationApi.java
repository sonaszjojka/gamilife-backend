package pl.gamilife.api.gamification;

import pl.gamilife.api.gamification.dto.GamificationUserDetails;

import java.util.UUID;

public interface GamificationApi {

    GamificationUserDetails getGamificationUserDetails(UUID userId);

}
