package pl.gamilife.gamification.api;

import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.gamification.usecase.getstartinggamificationvalues.GetStartingGamificationValuesCommand;
import pl.gamilife.gamification.usecase.getstartinggamificationvalues.GetStartingGamificationValuesUseCase;
import pl.gamilife.gamification.usecase.inituserstatistics.InitUserStatisticsCommand;
import pl.gamilife.gamification.usecase.inituserstatistics.InitUserStatisticsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
