package pl.gamilife.gamification.application.usecase.getuserstatistics;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;

import java.util.List;

@Component
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUserStatisticsUseCaseImpl implements GetUserStatisticsUseCase {
    private final UserStatisticRepository userStatisticRepository;

    @Override
    public List<GetUserStatisticsResult> execute(GetUserStatisticsCommand cmd) {
           List<UserStatistic> userStatisticList = userStatisticRepository.findByUserId(cmd.userId());
        return userStatisticList.stream().map(this::toResult).toList();
    }
    private GetUserStatisticsResult toResult(UserStatistic userStatistic) {
        return new GetUserStatisticsResult(
                userStatistic.getId(),
                new GetUserStatisticsResult.StatisticType(
                        userStatistic.getStatisticType().getId(),
                        userStatistic.getStatisticType().getType()
                ),
                userStatistic.getCount()
        );

    }
}
