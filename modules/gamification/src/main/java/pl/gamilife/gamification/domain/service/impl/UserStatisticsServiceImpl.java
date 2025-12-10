package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;
import pl.gamilife.gamification.domain.service.AchievementService;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class UserStatisticsServiceImpl implements UserStatisticsService {

    private final UserStatisticRepository userStatisticRepository;
    private final AchievementService achievementService;

    @Override
    public void registerProgressForAll(UUID userId, Set<StatisticTypeEnum> statisticTypes) {
        List<UserStatistic> userStatisticsToSave = new ArrayList<>();
        for (StatisticTypeEnum type : statisticTypes) {
            UserStatistic userStatistic = findOrCreateUserStatistic(userId, type);
            userStatistic.incrementCounterBy(1);
            userStatisticsToSave.add(userStatistic);
        }

        userStatisticRepository.saveAll(userStatisticsToSave);

        for (UserStatistic userStatistic : userStatisticsToSave) {
            achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);
        }
    }

    @Override
    public void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        UserStatistic userStatistic = findOrCreateUserStatistic(userId, statisticTypeEnum);
        userStatistic.incrementCounterBy(1);

        userStatisticRepository.save(userStatistic);
        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);
    }

    @Override
    public void registerProgressIfHigherThan(UUID userId, StatisticTypeEnum statisticTypeEnum, Integer newValue) {
        UserStatistic userStatistic = findOrCreateUserStatistic(userId, statisticTypeEnum);

        if (userStatistic.updateStreakIfHigher(newValue)) {
            log.debug(
                    "Updating statistic {} for user {} from {} to new value {}",
                    statisticTypeEnum,
                    userId,
                    userStatistic.getCount(),
                    newValue
            );

            userStatisticRepository.save(userStatistic);
            achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);
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
    public void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        userStatisticRepository
                .findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId())
                .ifPresent(us -> {
                    us.decrementCounterBy(1);
                    userStatisticRepository.save(us);
                });
    }

    private UserStatistic findOrCreateUserStatistic(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        return userStatisticRepository.findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId())
                .orElseGet(() -> {
                    log.warn("User has no statistic type: {}. Creating a new one.", statisticTypeEnum);
                    return UserStatistic.create(userId, statisticTypeEnum);
                });
    }
}
