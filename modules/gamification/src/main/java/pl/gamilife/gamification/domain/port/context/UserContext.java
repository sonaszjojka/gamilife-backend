package pl.gamilife.gamification.domain.port.context;

import pl.gamilife.gamification.domain.model.projection.GamificationUser;

import java.util.Optional;
import java.util.UUID;

public interface UserContext {
    Optional<GamificationUser> getUserById(UUID userId);

    void levelUpUser(UUID userId, int level);

    GamificationUser grantRewardsToUser(UUID userId, int experience, int money);

    int payForNewItem(UUID userId, int price);

    int refundUserAfterQuickSell(UUID userId, int quickSellValue);
}
