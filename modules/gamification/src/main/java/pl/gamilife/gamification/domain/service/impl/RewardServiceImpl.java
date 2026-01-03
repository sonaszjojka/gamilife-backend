package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.RewardRepository;
import pl.gamilife.gamification.domain.service.LevelService;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.shared.kernel.event.GamificationValuesChangedEvent;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class RewardServiceImpl implements RewardService {

    private final UserContext userContext;
    private final LevelService levelService;
    private final RewardRepository rewardRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        Optional<Reward> rewardOptional =
                rewardRepository.findByStatisticTypeId(statisticTypeEnum.getStatisticTypeId());

        if (rewardOptional.isEmpty()) {
            log.warn("Reward not found for statistic type: {}", statisticTypeEnum);
            return;
        }
        Reward reward = rewardOptional.get();

        rewardUser(userId, reward.getExperience(), reward.getMoney());
    }

    @Override
    public void rewardUser(UUID userId, int experience, int money) {
        GamificationUser rewardedUser = userContext.grantRewardsToUser(userId, experience, money);
        List<Level> gainedLevels = levelService.checkIfUserEligibleForLevelUp(
                rewardedUser.level(),
                rewardedUser.experience()
        );

        int userLevel = rewardedUser.level();
        if (!gainedLevels.isEmpty()) {
            userLevel = levelService.levelUpUser(rewardedUser, gainedLevels);
        }

        Optional<Level> nextLevel = levelService.getNextLevel(userLevel);
        eventPublisher.publishEvent(new GamificationValuesChangedEvent(
                rewardedUser.userId(),
                rewardedUser.username(),
                userLevel,
                rewardedUser.experience(),
                rewardedUser.money(),
                nextLevel.map(Level::getRequiredExperience).orElse(null)
        ));
    }

}
