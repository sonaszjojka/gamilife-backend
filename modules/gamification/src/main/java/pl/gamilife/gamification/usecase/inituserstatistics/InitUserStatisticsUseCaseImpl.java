package pl.gamilife.gamification.usecase.inituserstatistics;

import pl.gamilife.gamification.model.UserStatistic;
import pl.gamilife.gamification.repository.StatisticTypeRepository;
import pl.gamilife.gamification.repository.UserStatisticRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@Transactional
@AllArgsConstructor
public class InitUserStatisticsUseCaseImpl implements InitUserStatisticsUseCase {

    private final StatisticTypeRepository statisticTypeRepository;
    private final UserStatisticRepository userStatisticRepository;

    @Override
    public Void execute(InitUserStatisticsCommand cmd) {
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
