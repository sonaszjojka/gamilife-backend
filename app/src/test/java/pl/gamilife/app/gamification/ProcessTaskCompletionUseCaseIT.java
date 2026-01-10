package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.event.ApplicationEvents;
import org.springframework.test.context.event.RecordApplicationEvents;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionUseCase;
import pl.gamilife.gamification.domain.model.Achievement;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.StatisticType;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaAchievementRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaRewardRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaStatisticTypeRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserStatisticRepository;
import pl.gamilife.shared.kernel.event.AchievementUnlockedEvent;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@RecordApplicationEvents
class ProcessTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessTaskCompletionUseCase processTaskCompletionUseCase;

    @Autowired
    private JpaStatisticTypeRepository statisticTypeRepository;

    @Autowired
    private JpaRewardRepository rewardRepository;

    @Autowired
    private JpaUserStatisticRepository userStatisticRepository;

    @Autowired
    private JpaUserRepository userRepository;

    @Autowired
    private ApplicationEvents applicationEvents;

    @Autowired
    private JpaAchievementRepository achievementRepository;

    @Test
    @DisplayName("Should register progress for task completion and grant rewards")
    void shouldRegisterProgressForTaskCompletionAndGrantRewardsWhenNotGranted() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.COMPLETED_TASKS.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Reward reward = rewardRepository.findByStatisticTypeId(statisticType.getId()).orElseThrow();
        Achievement achievement = achievementRepository.findByName("First Task Completed").orElseThrow();

        // when
        processTaskCompletionUseCase.execute(
                new ProcessTaskCompletionCommand(user.getId(), false)
        );

        flushAndClear();

        // then
        List<AchievementUnlockedEvent> unlockedAchievements = applicationEvents.stream(AchievementUnlockedEvent.class).toList();

        assertThat(unlockedAchievements).hasSize(1);

        AchievementUnlockedEvent event = unlockedAchievements.getFirst();
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(event.achievementName()).isEqualTo(achievement.getName());
        assertThat(event.userId()).isEqualTo(user.getId());

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isGreaterThan(startingExp)
                .isGreaterThanOrEqualTo(startingExp + reward.getExperience());
        assertThat(user.getMoney()).isGreaterThan(startingMoney)
                .isGreaterThanOrEqualTo(startingMoney + reward.getMoney());
    }

    @Test
    @DisplayName("Should register progress for task completion and not grant rewards when already granted")
    void shouldRegisterProgressForTaskCompletionAndNotGrantRewardsWhenAlreadyGranted() {
        // given
        User user = createUserWithStats();
        StatisticType statisticType = statisticTypeRepository.findById(
                StatisticTypeEnum.COMPLETED_TASKS.getStatisticTypeId()
        ).orElseThrow();
        int startingExp = user.getExperience();
        int startingMoney = user.getMoney();
        Achievement achievement = achievementRepository.findByName("First Task Completed").orElseThrow();

        // when
        processTaskCompletionUseCase.execute(
                new ProcessTaskCompletionCommand(user.getId(), true)
        );

        flushAndClear();

        // then
        List<AchievementUnlockedEvent> unlockedAchievements = applicationEvents.stream(AchievementUnlockedEvent.class).toList();

        assertThat(unlockedAchievements).hasSize(1);

        AchievementUnlockedEvent event = unlockedAchievements.getFirst();
        user = userRepository.findById(user.getId()).orElseThrow();
        UserStatistic userStatistic = userStatisticRepository.findByUserIdAndStatisticTypeId(
                user.getId(), statisticType.getId()
        ).orElseThrow();

        assertThat(event.achievementName()).isEqualTo(achievement.getName());
        assertThat(event.userId()).isEqualTo(user.getId());

        assertThat(userStatistic.getCount()).isEqualTo(1);
        assertThat(user.getExperience()).isEqualTo(startingExp + achievement.getExperienceReward());
        assertThat(user.getMoney()).isEqualTo(startingMoney + achievement.getMoneyReward());
    }
}