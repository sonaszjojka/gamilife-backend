package pl.gamilife.gamification.domain.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import pl.gamilife.gamification.domain.model.Achievement;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.port.repository.AchievementRepository;
import pl.gamilife.gamification.domain.port.repository.UserAchievementRepository;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.gamification.domain.service.UserInventoryService;
import pl.gamilife.shared.kernel.event.AchievementUnlockedEvent;

import java.util.Optional;
import java.util.UUID;

import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AchievementServiceImplTest {

    @Mock
    private UserAchievementRepository userAchievementRepository;

    @Mock
    private AchievementRepository achievementRepository;

    @Mock
    private UserInventoryService userInventoryService;

    @Mock
    private RewardService rewardService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @InjectMocks
    private AchievementServiceImpl achievementService;

    @Test
    void shouldAssignAchievement_whenUserQualifies() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;

        Achievement achievement = Instancio.of(Achievement.class)
                .set(field(Achievement::getGoal), 10)
                .set(field(Achievement::getStatisticTypeId), type.getStatisticTypeId())
                .create();

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), 15)
                .create();

        when(achievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(anyInt(), any(UUID.class)))
                .thenReturn(Optional.of(achievement));

        // when
        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);

        // then
        verify(userAchievementRepository).save(any());
        verify(userInventoryService).addItemsToUsersInventory(eq(userId), any());
        verify(rewardService).rewardUser(eq(userId), anyInt(), anyInt());
        verify(eventPublisher).publishEvent(any(AchievementUnlockedEvent.class));
    }

    @Test
    void shouldDoNothing_whenUserAlreadyHasAllAchievements() {
        // given
        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getStatisticTypeId), StatisticTypeEnum.COMPLETED_TASKS.getStatisticTypeId())
                .create();

        when(achievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(anyInt(), any(UUID.class)))
                .thenReturn(Optional.empty());

        // when
        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);

        // then
        verifyNoInteractions(userAchievementRepository);
        verifyNoInteractions(userInventoryService);
        verifyNoInteractions(rewardService);
        verifyNoInteractions(eventPublisher);
    }

    @Test
    void shouldDoNothing_whenUserDoesNotMeetGoal() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;

        Achievement achievement = Instancio.of(Achievement.class)
                .set(field(Achievement::getGoal), 100)
                .set(field(Achievement::getStatisticTypeId), type.getStatisticTypeId())
                .create();

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), 50)
                .create();

        when(achievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(anyInt(), any(UUID.class)))
                .thenReturn(Optional.of(achievement));

        // when
        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);

        // then
        verifyNoInteractions(userAchievementRepository);
    }
}
