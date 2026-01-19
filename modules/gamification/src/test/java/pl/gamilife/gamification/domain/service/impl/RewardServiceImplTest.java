package pl.gamilife.gamification.domain.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.RewardRepository;
import pl.gamilife.gamification.domain.service.LevelService;
import pl.gamilife.shared.kernel.event.GamificationValuesChangedEvent;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RewardServiceImplTest {

    @Mock
    private UserContext userContext;

    @Mock
    private LevelService levelService;

    @Mock
    private RewardRepository rewardRepository;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @InjectMocks
    private RewardServiceImpl rewardService;

    @Test
    void shouldRewardUserByStatisticType_whenRewardExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        Reward reward = Instancio.of(Reward.class)
                .set(field(Reward::getExperience), 100)
                .set(field(Reward::getMoney), 50)
                .create();

        GamificationUser user = Instancio.create(GamificationUser.class);

        when(rewardRepository.findByStatisticTypeId(type.getStatisticTypeId())).thenReturn(Optional.of(reward));
        when(userContext.grantRewardsToUser(userId, 100, 50)).thenReturn(user);
        when(levelService.checkIfUserEligibleForLevelUp(anyInt(), anyInt())).thenReturn(Collections.emptyList());
        when(levelService.getNextLevel(anyInt())).thenReturn(Optional.empty());

        // when
        rewardService.rewardUser(userId, type);

        // then
        verify(userContext).grantRewardsToUser(userId, 100, 50);
        verify(eventPublisher).publishEvent(any(GamificationValuesChangedEvent.class));
    }

    @Test
    void shouldNotRewardUser_whenRewardDoesNotExistForStatisticType() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;

        when(rewardRepository.findByStatisticTypeId(type.getStatisticTypeId())).thenReturn(Optional.empty());

        // when
        rewardService.rewardUser(userId, type);

        // then
        verifyNoInteractions(userContext);
        verifyNoInteractions(levelService);
        verifyNoInteractions(eventPublisher);
    }

    @Test
    void shouldRewardUserWithValuesAndLevelUp_whenUserIsEligibleForLevelUp() {
        // given
        UUID userId = UUID.randomUUID();
        int exp = 100;
        int money = 50;

        GamificationUser userAfterRewards = Instancio.of(GamificationUser.class)
                .set(field(GamificationUser::userId), userId)
                .set(field(GamificationUser::level), 1)
                .set(field(GamificationUser::experience), 200)
                .create();

        Level nextLevel = Instancio.create(Level.class);
        List<Level> gainedLevels = List.of(nextLevel);

        GamificationUser userAfterLevelUp = Instancio.of(GamificationUser.class)
                .set(field(GamificationUser::userId), userId)
                .set(field(GamificationUser::level), 2)
                .create();

        when(userContext.grantRewardsToUser(userId, exp, money)).thenReturn(userAfterRewards);
        when(levelService.checkIfUserEligibleForLevelUp(1, 200)).thenReturn(gainedLevels);
        when(levelService.levelUpUser(userAfterRewards, gainedLevels)).thenReturn(userAfterLevelUp);
        when(levelService.getNextLevel(2)).thenReturn(Optional.empty());

        // when
        rewardService.rewardUser(userId, exp, money);

        // then
        verify(levelService).levelUpUser(userAfterRewards, gainedLevels);
        verify(eventPublisher).publishEvent(any(GamificationValuesChangedEvent.class));
    }

    @Test
    void shouldRewardUserWithValuesWithoutLevelUp_whenUserIsNotEligibleForLevelUp() {
        // given
        UUID userId = UUID.randomUUID();
        GamificationUser user = Instancio.create(GamificationUser.class);

        when(userContext.grantRewardsToUser(any(), anyInt(), anyInt())).thenReturn(user);
        when(levelService.checkIfUserEligibleForLevelUp(anyInt(), anyInt())).thenReturn(Collections.emptyList());
        when(levelService.getNextLevel(anyInt())).thenReturn(Optional.empty());

        // when
        rewardService.rewardUser(userId, 10, 10);

        // then
        verify(levelService, never()).levelUpUser(any(), any());
        verify(eventPublisher).publishEvent(any(GamificationValuesChangedEvent.class));
    }

    @Test
    void shouldPublishEventWithNullNextLevelExp_whenMaxLevelReached() {
        // given
        UUID userId = UUID.randomUUID();
        GamificationUser user = Instancio.create(GamificationUser.class);

        when(userContext.grantRewardsToUser(any(), anyInt(), anyInt())).thenReturn(user);
        when(levelService.checkIfUserEligibleForLevelUp(anyInt(), anyInt())).thenReturn(Collections.emptyList());
        when(levelService.getNextLevel(anyInt())).thenReturn(Optional.empty());

        // when
        rewardService.rewardUser(userId, 10, 10);

        // then
        verify(eventPublisher).publishEvent(any(GamificationValuesChangedEvent.class));
    }
}
