package pl.gamilife.gamification.domain.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;
import pl.gamilife.gamification.domain.service.AchievementService;

import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UserStatisticsServiceImplTest {

    @Mock
    private UserStatisticRepository userStatisticRepository;

    @Mock
    private AchievementService achievementService;

    @InjectMocks
    private UserStatisticsServiceImpl userStatisticsService;

    @Test
    void shouldRegisterSingleProgress_whenUserStatisticExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int initialCount = 10;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.registerProgress(userId, type);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(initialCount + 1);
        verify(userStatisticRepository).save(userStatistic);
        verify(achievementService).checkIfUserQualifiesForAchievementOfType(userStatistic);
    }

    @Test
    void shouldRegisterProgress_whenUserStatisticExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int initialCount = 10;
        int progress = 5;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.registerProgress(userId, type, progress);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(initialCount + progress);
        verify(userStatisticRepository).save(userStatistic);
        verify(achievementService).checkIfUserQualifiesForAchievementOfType(userStatistic);
    }

    @Test
    void shouldRegisterProgressAndCreateStatistic_whenUserStatisticDoesNotExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int progress = 5;

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.empty());

        // when
        userStatisticsService.registerProgress(userId, type, progress);

        // then
        verify(userStatisticRepository).save(argThat(us ->
                us.getUserId().equals(userId) &&
                        us.getStatisticTypeId().equals(type.getStatisticTypeId()) &&
                        us.getCount() == progress
        ));
        verify(achievementService).checkIfUserQualifiesForAchievementOfType(any(UserStatistic.class));
    }

    @Test
    void shouldThrowExceptionDuringRegisterProgress_whenProgressIsNegative() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int progress = -5;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), 10)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        Throwable thrown = catchThrowable(() -> userStatisticsService.registerProgress(userId, type, progress));

        // then
        assertThat(thrown).isInstanceOf(IllegalArgumentException.class);
        verify(userStatisticRepository, never()).save(any());
    }

    @Test
    void shouldUpdateStreak_whenNewValueIsHigher() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.HABIT_STREAK;
        int initialCount = 5;
        int newValue = 10;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.registerProgressIfHigherThan(userId, type, newValue);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(newValue);
        verify(userStatisticRepository).save(userStatistic);
        verify(achievementService).checkIfUserQualifiesForAchievementOfType(userStatistic);
    }

    @Test
    void shouldNotUpdateStreak_whenNewValueIsNotHigher() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.HABIT_STREAK;
        int initialCount = 10;
        int newValue = 5;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.registerProgressIfHigherThan(userId, type, newValue);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(initialCount);
        verify(userStatisticRepository, never()).save(any());
        verify(achievementService, never()).checkIfUserQualifiesForAchievementOfType(any());
    }

    @Test
    void shouldThrowExceptionDuringUpdateStreak_whenNewValueIsNegative() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.HABIT_STREAK;
        int newValue = -1;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), 5)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        Throwable thrown = catchThrowable(() -> userStatisticsService.registerProgressIfHigherThan(userId, type, newValue));

        // then
        assertThat(thrown).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void shouldRollbackProgress_whenStatisticExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int initialCount = 10;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.rollbackProgress(userId, type);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(initialCount - 1);
        verify(userStatisticRepository).save(userStatistic);
    }

    @Test
    void shouldNotRollbackProgress_whenStatisticDoesNotExists() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.empty());

        // when
        userStatisticsService.rollbackProgress(userId, type);

        // then
        verify(userStatisticRepository, never()).save(any());
    }

    @Test
    void shouldRollbackProgressToZero_whenCurrentCountIsOne() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum type = StatisticTypeEnum.COMPLETED_TASKS;
        int initialCount = 1;

        UserStatistic userStatistic = Instancio.of(UserStatistic.class)
                .set(field(UserStatistic::getUserId), userId)
                .set(field(UserStatistic::getStatisticTypeId), type.getStatisticTypeId())
                .set(field(UserStatistic::getCount), initialCount)
                .create();

        when(userStatisticRepository.findByUserIdAndStatisticTypeId(userId, type.getStatisticTypeId()))
                .thenReturn(Optional.of(userStatistic));

        // when
        userStatisticsService.rollbackProgress(userId, type);

        // then
        assertThat(userStatistic.getCount()).isZero();
        verify(userStatisticRepository).save(userStatistic);
    }
}
