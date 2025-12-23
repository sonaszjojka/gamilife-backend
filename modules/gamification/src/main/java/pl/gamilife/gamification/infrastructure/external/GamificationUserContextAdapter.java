package pl.gamilife.gamification.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.api.user.dto.RewardedUserApiDto;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;

import java.util.Optional;
import java.util.UUID;

@Component
@AllArgsConstructor
public class GamificationUserContextAdapter implements UserContext {

    private final UserApi userApi;

    @Override
    public Optional<GamificationUser> getUserById(UUID userId) {
        Optional<BasicUserInfoDto> userById = userApi.getUserById(userId);

        if (userById.isEmpty()) {
            return Optional.empty();
        }

        BasicUserInfoDto user = userById.get();

        return Optional.of(new GamificationUser(
                user.userId(),
                user.username(),
                user.level(),
                user.experience(),
                user.money()
        ));
    }

    @Override
    public void levelUpUser(UUID userId, int level) {
        userApi.levelUpUser(userId, level);
    }

    @Override
    public GamificationUser grantRewardsToUser(UUID userId, int experience, int money) {
        RewardedUserApiDto rewardedUser = userApi.grantRewardsToUser(userId, experience, money);

        return new GamificationUser(
                rewardedUser.userId(),
                rewardedUser.username(),
                rewardedUser.level(),
                rewardedUser.experience(),
                rewardedUser.money()
        );
    }

    @Override
    public int payForNewItem(UUID userId, int price) {
        return userApi.editUserMoneyBy(userId, (-1) * price);
    }

    @Override
    public int refundUserAfterQuickSell(UUID userId, int quickSellValue) {
        return userApi.editUserMoneyBy(userId, quickSellValue);
    }
}
