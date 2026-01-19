package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class StatisticTypeTest {

    @Test
    void shouldReturnCorrectStatisticTypeEnum_whenValidIdIsProvided() {
        // given
        StatisticTypeEnum expectedEnum = StatisticTypeEnum.COMPLETED_TASKS;
        StatisticType statisticType = Instancio.of(StatisticType.class)
                .set(field(StatisticType::getId), expectedEnum.getStatisticTypeId())
                .create();

        // when
        StatisticTypeEnum result = statisticType.getStatisticTypeEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenStatisticTypeIdIsInvalid() {
        // given
        int invalidId = 999;
        StatisticType statisticType = Instancio.of(StatisticType.class)
                .set(field(StatisticType::getId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(statisticType::getStatisticTypeEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Invalid statisticTypeId: " + invalidId);
    }

    @Test
    void shouldThrowException_whenStatisticTypeIdIsNull() {
        // given
        StatisticType statisticType = Instancio.of(StatisticType.class)
                .set(field(StatisticType::getId), null)
                .create();

        // when
        Throwable thrown = catchThrowable(statisticType::getStatisticTypeEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(NullPointerException.class);
    }
}
