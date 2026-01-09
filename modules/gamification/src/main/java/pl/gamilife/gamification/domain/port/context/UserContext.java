package pl.gamilife.gamification.domain.port.context;

import pl.gamilife.gamification.domain.model.projection.GamificationUser;

import java.util.Optional;
import java.util.UUID;

public interface UserContext {
    Optional<GamificationUser> getUserById(UUID userId);

    GamificationUser levelUpUser(UUID userId, int level);

    GamificationUser grantRewardsToUser(UUID userId, int experience, int money);

    GamificationUser payForNewItem(UUID userId, int price);

    GamificationUser refundUserAfterQuickSell(UUID userId, int quickSellValue);
}
