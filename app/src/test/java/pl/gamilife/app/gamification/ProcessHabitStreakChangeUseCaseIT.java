package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.processhabitstreakchange.ProcessHabitStreakChangeCommand;
import pl.gamilife.gamification.application.usecase.processhabitstreakchange.ProcessHabitStreakChangeUseCase;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.StatisticType;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaRewardRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaStatisticTypeRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserStatisticRepository;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessHabitStreakChangeUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessHabitStreakChangeUseCase processHabitStreakChangeUseCase;

    @Autowired
    private JpaStatisticTypeRepository statisticTypeRepository;

    @Autowired
    private JpaRewardRepository rewardRepository;

    @Autowired
    private JpaUserStatisticRepository userStatisticRepository;

    @Autowired
    private JpaUserRepository userRepository;

    @Test
    @DisplayName("Should update habit streak if higher than current")
    void shouldUpdateHabitStreakIfHigher() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.HABIT_STREAK.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();

        // when
        processHabitStreakChangeUseCase.execute(new ProcessHabitStreakChangeCommand(user.getId(), 5));

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(5);
        assertThat(user.getExperience()).isGreaterThan(startingExp)
                .isGreaterThanOrEqualTo(startingExp + reward.getExperience());
        assertThat(user.getMoney()).isGreaterThan(startingMoney)
                .isGreaterThanOrEqualTo(startingMoney + reward.getMoney());

    }

    @Test
    @DisplayName("Should not update habit streak if lower than current")
    void shouldNotUpdateHabitStreakIfLower() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.HABIT_STREAK.getStatisticTypeId()
        ).orElseThrow();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();
        processHabitStreakChangeUseCase.execute(new ProcessHabitStreakChangeCommand(user.getId(), 5));

        flushAndClear();

        user = userRepository.findById(user.getId()).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();

        // when
        processHabitStreakChangeUseCase.execute(new ProcessHabitStreakChangeCommand(user.getId(), 2));

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(5);
        assertThat(user.getExperience()).isGreaterThan(startingExp)
                .isGreaterThanOrEqualTo(startingExp + reward.getExperience());
        assertThat(user.getMoney()).isGreaterThan(startingMoney)
                .isGreaterThanOrEqualTo(startingMoney + reward.getMoney());
    }
}
