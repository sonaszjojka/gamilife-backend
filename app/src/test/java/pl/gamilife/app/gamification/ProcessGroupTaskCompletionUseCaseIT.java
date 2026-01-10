package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionUseCase;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.StatisticType;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaRewardRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaStatisticTypeRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserStatisticRepository;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessGroupTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessGroupTaskCompletionUseCase processGroupTaskCompletionUseCase;

    @Autowired
    private JpaStatisticTypeRepository statisticTypeRepository;

    @Autowired
    private JpaRewardRepository rewardRepository;

    @Autowired
    private JpaUserStatisticRepository userStatisticRepository;

    @Autowired
    private JpaUserRepository userRepository;

    @Test
    @DisplayName("Should register progress for group task completion and grant rewards when not granted")
    void shouldRegisterProgressForGroupTaskCompletion() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.GROUP_TASKS_COMPLETED.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();

        // when
        processGroupTaskCompletionUseCase.execute(new ProcessGroupTaskCompletionCommand(Collections.singleton(user.getId()), false));

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isGreaterThan(startingExp)
                .isGreaterThanOrEqualTo(startingExp + reward.getExperience());

        if (reward.getMoney() > 0) {
            assertThat(user.getMoney()).isGreaterThanOrEqualTo(startingMoney + reward.getMoney());
        } else {
            assertThat(user.getMoney()).isEqualTo(startingMoney);
        }
    }

    @Test
    @DisplayName("Should register progress for group task completion and not grant rewards when already granted")
    void shouldRegisterProgressForGroupTaskCompletionAndNotGrantRewardsWhenAlreadyGranted() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.GROUP_TASKS_COMPLETED.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();

        // when
        processGroupTaskCompletionUseCase.execute(
                new ProcessGroupTaskCompletionCommand(Collections.singleton(user.getId()), true)
        );

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isLessThan(startingExp + reward.getExperience());

        if (reward.getMoney() > 0) {
            assertThat(user.getMoney()).isLessThan(startingMoney + reward.getMoney());
        } else {
            assertThat(user.getMoney()).isEqualTo(startingMoney);
        }
    }
}
