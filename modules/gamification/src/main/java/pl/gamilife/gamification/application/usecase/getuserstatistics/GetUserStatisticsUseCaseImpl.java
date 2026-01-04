package pl.gamilife.gamification.application.usecase.getuserstatistics;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.UserStatisticRepository;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

@Component
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUserStatisticsUseCaseImpl implements GetUserStatisticsUseCase {
    private final UserStatisticRepository userStatisticRepository;

    @Override
    public GetUserStatisticsResult execute(GetUserStatisticsCommand cmd) {
            UserStatistic userStatistic = userStatisticRepository.findByUserId(cmd.userId())
                    .orElseThrow(()->new UserNotFoundException("User not found"));
        return this.toResult(userStatistic);
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
