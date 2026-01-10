package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class AchievementTest {

    @Test
    void shouldReturnCorrectStatisticTypeEnum_whenValidIdIsProvided() {
        // given
        StatisticTypeEnum expectedEnum = StatisticTypeEnum.COMPLETED_TASKS;
        Achievement achievement = Instancio.of(Achievement.class)
                .set(field(Achievement::getStatisticTypeId), expectedEnum.getStatisticTypeId())
                .create();

        // when
        StatisticTypeEnum result = achievement.getStatisticTypeEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenStatisticTypeIdIsInvalid() {
        // given
        int invalidId = 999;
        Achievement achievement = Instancio.of(Achievement.class)
                .set(field(Achievement::getStatisticTypeId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(achievement::getStatisticTypeEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Invalid statisticTypeId: " + invalidId);
    }

    @Test
    void shouldThrowException_whenStatisticTypeIdIsNull() {
        // given
        Achievement achievement = Instancio.of(Achievement.class)
                .set(field(Achievement::getStatisticTypeId), null)
                .create();

        // when
        Throwable thrown = catchThrowable(achievement::getStatisticTypeEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(NullPointerException.class);
    }
}
