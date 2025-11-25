package edu.pjwstk.gamification.service.impl;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.model.UserStatistic;
import edu.pjwstk.gamification.repository.UserStatisticRepository;
import edu.pjwstk.gamification.service.AchievementService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class UserStatisticsServiceImpl implements UserStatisticsService {

    private UserStatisticRepository userStatisticRepository;
    private AchievementService achievementService;

    @Override
    @Transactional
    public void registerProgressForAll(UUID userId, Set<StatisticTypeEnum> statisticTypes) {
        statisticTypes.forEach(type -> registerProgress(userId, type));
    }

    @Override
    @Transactional
    public void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        Optional<UserStatistic> userStatisticOptional =
                userStatisticRepository.findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId());

        UserStatistic userStatistic;
        if (userStatisticOptional.isEmpty()) {
            log.warn("User has no statistic type: {}. Creating new one.", statisticTypeEnum);
            userStatistic = UserStatistic.builder()
                    .userId(userId)
                    .statisticTypeId(statisticTypeEnum.getStatisticTypeId())
                    .count(1)
                    .build();
        } else {
            userStatistic = userStatisticOptional.get();
            userStatistic.setCount(userStatistic.getCount() + 1);
        }

        userStatisticRepository.save(userStatistic);
        log.info("User progressed by 1 in {}", statisticTypeEnum);

        achievementService.checkIfUserQualifiesForAchievementOfType(userStatistic);
    }

    @Override
    public void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        userStatisticRepository
                .findByUserIdAndStatisticTypeId(userId, statisticTypeEnum.getStatisticTypeId())
                .ifPresent(us -> {
                    us.setCount(us.getCount() - 1);
                    userStatisticRepository.save(us);
                    log.warn("User progressed by -1 in {}.", statisticTypeEnum);
                });
    }
}
