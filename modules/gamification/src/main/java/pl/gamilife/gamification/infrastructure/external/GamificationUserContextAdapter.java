package pl.gamilife.gamification.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
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

        return Optional.of(buildGamificationUser(user));
    }

    @Override
    public GamificationUser levelUpUser(UUID userId, int level) {
        BasicUserInfoDto user = userApi.levelUpUser(userId, level);

        return buildGamificationUser(user);
    }

    @Override
    public GamificationUser grantRewardsToUser(UUID userId, int experience, int money) {
        BasicUserInfoDto user = userApi.grantRewardsToUser(userId, experience, money);

        return buildGamificationUser(user);
    }

    @Override
    public GamificationUser payForNewItem(UUID userId, int price) {
        BasicUserInfoDto user = userApi.editUserMoneyBy(userId, (-1) * price);
        return buildGamificationUser(user);
    }

    @Override
    public GamificationUser refundUserAfterQuickSell(UUID userId, int quickSellValue) {
        BasicUserInfoDto user = userApi.editUserMoneyBy(userId, quickSellValue);
        return buildGamificationUser(user);
    }

    private GamificationUser buildGamificationUser(BasicUserInfoDto user) {
        return new GamificationUser(
                user.userId(),
                user.username(),
                user.level(),
                user.experience(),
                user.money(),
                user.statsVersion()
        );
    }
}
