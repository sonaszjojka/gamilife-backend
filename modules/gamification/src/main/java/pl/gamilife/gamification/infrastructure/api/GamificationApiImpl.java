package pl.gamilife.gamification.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.gamification.application.usecase.getstartinggamificationvalues.GetStartingGamificationValuesCommand;
import pl.gamilife.gamification.application.usecase.getstartinggamificationvalues.GetStartingGamificationValuesUseCase;
import pl.gamilife.gamification.application.usecase.inituserstatistics.InitUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.inituserstatistics.InitUserStatisticsUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GamificationApiImpl implements GamificationApi {

    private final InitUserStatisticsUseCase initUserStatisticsUseCase;
    private final GetStartingGamificationValuesUseCase getStartingGamificationValuesUseCase;

    @Override
    public void initUserStatisticsFor(UUID userId) {
        initUserStatisticsUseCase.execute(new InitUserStatisticsCommand(userId));
    }

    @Override
    public StartingGamificationValuesDto getStartingGamificationValues() {
        return getStartingGamificationValuesUseCase.execute(new GetStartingGamificationValuesCommand());
    }
}
