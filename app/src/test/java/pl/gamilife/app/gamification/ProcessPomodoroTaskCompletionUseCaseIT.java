package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionUseCase;
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

class ProcessPomodoroTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessPomodoroTaskCompletionUseCase processPomodoroTaskCompletionUseCase;

    @Autowired
    private JpaStatisticTypeRepository statisticTypeRepository;

    @Autowired
    private JpaRewardRepository rewardRepository;

    @Autowired
    private JpaUserStatisticRepository userStatisticRepository;

    @Autowired
    private JpaUserRepository userRepository;

    @Test
    @DisplayName("Should register progress for pomodoro task completion and grant rewards when not granted")
    void shouldRegisterProgressForPomodoroTaskCompletion() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.POMODORO_TASKS_COMPLETED.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();

        // when
        processPomodoroTaskCompletionUseCase.execute(new ProcessPomodoroTaskCompletionCommand(user.getId(), false));

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isGreaterThan(startingExp)
                .isGreaterThanOrEqualTo(startingExp + reward.getExperience());
        assertThat(user.getMoney()).isGreaterThanOrEqualTo(startingMoney + reward.getMoney());
    }

    @Test
    @DisplayName("Should register progress for pomodoro task completion and not grant rewards when already granted")
    void shouldRegisterProgressForTaskCompletionAndNotGrantRewards_whenAlreadyGranted() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.POMODORO_TASKS_COMPLETED.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();

        // when
        processPomodoroTaskCompletionUseCase.execute(
                new ProcessPomodoroTaskCompletionCommand(user.getId(), true)
        );

        flushAndClear();

        // then
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isLessThan(startingExp + reward.getExperience());
        assertThat(user.getMoney()).isLessThan(startingMoney + reward.getMoney());
    }
}