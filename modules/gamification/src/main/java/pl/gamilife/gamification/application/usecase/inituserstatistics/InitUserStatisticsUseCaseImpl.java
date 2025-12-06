package pl.gamilife.gamification.application.usecase.inituserstatistics;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.StatisticTypeRepository;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional
@AllArgsConstructor
public class InitUserStatisticsUseCaseImpl implements InitUserStatisticsUseCase {

    private final StatisticTypeRepository statisticTypeRepository;
    private final UserStatisticRepository userStatisticRepository;

    @Override
    public Void execute(InitUserStatisticsCommand cmd) {
        List<UserStatistic> userStatistics = statisticTypeRepository.findAll()
                .stream()
                .map(statisticType -> UserStatistic.builder()
                        .statisticTypeId(statisticType.getId())
                        .userId(cmd.userId())
                        .build()
                )
                .collect(Collectors.toList());

        userStatisticRepository.saveAll(userStatistics);

        return null;
    }
}
