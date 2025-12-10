package pl.gamilife.gamification.application.usecase.processuserregistered;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.StatisticTypeRepository;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;

import java.util.List;

@Service
@Transactional
@AllArgsConstructor
public class ProcessUserRegisteredUseCaseImpl implements ProcessUserRegisteredUseCase {

    private final StatisticTypeRepository statisticTypeRepository;
    private final UserStatisticRepository userStatisticRepository;

    @Override
    public Void execute(ProcessUserRegisteredCommand cmd) {
        List<UserStatistic> userStatistics = statisticTypeRepository.findAll()
                .stream()
                .map(statisticType -> UserStatistic.create(
                        cmd.userId(),
                        statisticType.getStatisticTypeEnum()
                ))
                .toList();

        userStatisticRepository.saveAll(userStatistics);

        return null;
    }
}
