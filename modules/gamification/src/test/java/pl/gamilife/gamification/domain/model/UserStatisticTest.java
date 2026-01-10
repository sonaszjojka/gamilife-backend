package pl.gamilife.gamification.domain.model;

import org.junit.jupiter.api.Test;
import pl.gamilife.gamification.domain.exception.InvalidGamificationOperationException;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class UserStatisticTest {

    @Test
    void shouldCreateUserStatistic_whenValidDataIsProvided() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum statisticType = StatisticTypeEnum.COMPLETED_TASKS;

        // when
        UserStatistic result = UserStatistic.create(userId, statisticType);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getUserId()).isEqualTo(userId);
        assertThat(result.getStatisticTypeEnum()).isEqualTo(statisticType);
        assertThat(result.getCount()).isZero();
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenUserIdIsNull() {
        // given
        UUID userId = null;
        StatisticTypeEnum statisticType = StatisticTypeEnum.COMPLETED_TASKS;

        // when
        Throwable thrown = catchThrowable(() -> UserStatistic.create(userId, statisticType));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User ID cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenStatisticTypeIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        StatisticTypeEnum statisticType = null;

        // when
        Throwable thrown = catchThrowable(() -> UserStatistic.create(userId, statisticType));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Statistic type cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenUserIdAndStatisticTypeAreNull() {
        // given
        UUID userId = null;
        StatisticTypeEnum statisticType = null;

        // when
        Throwable thrown = catchThrowable(() -> UserStatistic.create(userId, statisticType));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User ID cannot be null");
    }

    @Test
    void shouldReturnCorrectStatisticTypeEnum_whenValidIdIsStored() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);

        // when
        StatisticTypeEnum result = userStatistic.getStatisticTypeEnum();

        // then
        assertThat(result).isEqualTo(StatisticTypeEnum.HABIT_STREAK);
    }

    @Test
    void shouldIncrementCounter_whenValidAmountAndNonStreakStatistic() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);
        int amount = 5;

        // when
        userStatistic.incrementCounterBy(amount);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(5);
    }

    @Test
    void shouldThrowExceptionDuringIncrement_whenAmountIsZero() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.incrementCounterBy(0));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Counter increment be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringIncrement_whenAmountIsNegative() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.incrementCounterBy(-5));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Counter increment be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringIncrement_whenStatisticIsStreakType() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.incrementCounterBy(1));

        // then
        assertThat(thrown)
                .isInstanceOf(InvalidGamificationOperationException.class)
                .hasMessage("Only non-streak statistics could be processed with this method.");
    }

    @Test
    void shouldDecrementCounter_whenValidAmountAndNonStreakStatistic() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);
        userStatistic.incrementCounterBy(10);
        int amount = 4;

        // when
        userStatistic.decrementCounterBy(amount);

        // then
        assertThat(userStatistic.getCount()).isEqualTo(6);
    }

    @Test
    void shouldThrowExceptionDuringDecrement_whenAmountIsZero() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.decrementCounterBy(0));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Counter decrement must be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringDecrement_whenAmountIsNegative() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.decrementCounterBy(-1));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Counter decrement must be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringDecrement_whenStatisticIsStreakType() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.decrementCounterBy(1));

        // then
        assertThat(thrown)
                .isInstanceOf(InvalidGamificationOperationException.class)
                .hasMessage("Only non-streak statistics could be processed with this method.");
    }

    @Test
    void shouldUpdateStreak_whenNewCountIsHigherAndStreakStatistic() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);
        int newCount = 5;

        // when
        boolean result = userStatistic.updateStreakIfHigher(newCount);

        // then
        assertThat(result).isTrue();
        assertThat(userStatistic.getCount()).isEqualTo(5);
    }

    @Test
    void shouldNotUpdateStreak_whenNewCountIsNotHigherAndStreakStatistic() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);
        userStatistic.updateStreakIfHigher(5);
        int lowerCount = 3;

        // when
        boolean result = userStatistic.updateStreakIfHigher(lowerCount);

        // then
        assertThat(result).isFalse();
        assertThat(userStatistic.getCount()).isEqualTo(5);
    }

    @Test
    void shouldThrowExceptionDuringUpdateStreak_whenNewCountIsZero() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.HABIT_STREAK);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.updateStreakIfHigher(0));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("New streak value must be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringUpdateStreak_whenStatisticIsNotStreakType() {
        // given
        UserStatistic userStatistic = UserStatistic.create(UUID.randomUUID(), StatisticTypeEnum.COMPLETED_TASKS);

        // when
        Throwable thrown = catchThrowable(() -> userStatistic.updateStreakIfHigher(1));

        // then
        assertThat(thrown)
                .isInstanceOf(InvalidGamificationOperationException.class)
                .hasMessage("Only streak statistics could be processed with this method.");
    }
}
