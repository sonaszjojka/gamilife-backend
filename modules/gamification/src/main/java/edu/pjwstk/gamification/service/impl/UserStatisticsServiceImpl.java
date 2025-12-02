package edu.pjwstk.gamification.service.impl;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.model.UserStatistic;
import edu.pjwstk.gamification.repository.UserStatisticRepository;
import edu.pjwstk.gamification.service.AchievementService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class UserStatisticsServiceImpl implements UserStatisticsService {

    private final UserStatisticRepository userStatisticRepository;
    private final AchievementService achievementService;

    @Override
    @Transactional
    public void registerProgressForAll(UUID userId, Set<StatisticTypeEnum> statisticTypes) {
        statisticTypes.forEach(type -> registerProgress(userId, type));
    }

    @Override
    @Transactional
    public void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        UserStatistic userStatistic = findOrCreateUserStatistic(userId, statisticTypeEnum);
        userStatistic.setCount(userStatistic.getCount() + 1);
        saveAndCheckAchievements(userStatistic);
    }

    @Override
    @Transactional
    public void registerProgressIfHigherThan(UUID userId, StatisticTypeEnum statisticTypeEnum, Integer newValue) {
        UserStatistic userStatistic = findOrCreateUserStatistic(userId, statisticTypeEnum);

        if (newValue > userStatistic.getCount()) {
            log.debug(
                    "Updating statistic {} for user {} from {} to new value {}",
                    statisticTypeEnum,
                    userId,
                    userStatistic.getCount(),
                    newValue
            );
            userStatistic.setCount(newValue);
            saveAndCheckAchievements(userStatistic);
        } else {
            log.debug(
                    "Statistic {} for user {} with current value {} is not updated as new value {} is not higher.",
                    statisticTypeEnum,
                    userId,
                    userStatistic.getCount(),
                    newValue
            );
        }
    }

    @Override
    @Transactional
    public void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        userStatisticRepository
                .findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId())
                .ifPresent(us -> {
                    us.setCount(us.getCount() - 1);
                    saveAndCheckAchievements(us);
                });
    }

    private UserStatistic findOrCreateUserStatistic(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        return userStatisticRepository.findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId())
                .orElseGet(() -> {
                    log.warn("User has no statistic type: {}. Creating a new one.", statisticTypeEnum);
                    return UserStatistic.builder()
                            .userId(userId)
                            .statisticTypeId(statisticTypeEnum.getStatisticTypeId())
                            .count(0)
                            .build();
                });
    }

    private void saveAndCheckAchievements(UserStatistic userStatistic) {
        userStatisticRepository.save(userStatistic);

        log.debug(
                "User statistic {} for user {} changed. New count: {}",
                StatisticTypeEnum.fromId(userStatistic.getStatisticTypeId()),
                userStatistic.getUserId(),
                userStatistic.getCount()
        );

        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);
    }
}
