package edu.pjwstk.gamification.usecase.inituserstatistics;

import edu.pjwstk.gamification.model.UserStatistic;
import edu.pjwstk.gamification.repository.StatisticTypeRepository;
import edu.pjwstk.gamification.repository.UserStatisticRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
@AllArgsConstructor
public class InitUserStatisticsUseCaseImpl implements InitUserStatisticsUseCase {

    private final StatisticTypeRepository statisticTypeRepository;
    private final UserStatisticRepository userStatisticRepository;

    @Override
    @Transactional
    public Void executeInternal(InitUserStatisticsCommand cmd) {
        List<UserStatistic> userStatistics = new ArrayList<>();
        statisticTypeRepository.findAll().forEach(statisticType -> {
            UserStatistic userStatistic = UserStatistic.builder()
                    .statisticTypeId(statisticType.getId())
                    .userId(cmd.userId())
                    .build();
            userStatistics.add(userStatistic);
        });

        userStatisticRepository.saveAll(userStatistics);

        return null;
    }
}
