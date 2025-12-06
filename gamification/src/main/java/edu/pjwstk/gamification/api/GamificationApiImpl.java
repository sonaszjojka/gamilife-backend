package edu.pjwstk.gamification.api;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.api.gamification.dto.StartingGamificationValuesDto;
import edu.pjwstk.gamification.usecase.getrequiredexperiencebylevelid.GetRequiredExperienceByLevelIdCommand;
import edu.pjwstk.gamification.usecase.getrequiredexperiencebylevelid.GetRequiredExperienceByLevelIdUseCase;
import edu.pjwstk.gamification.usecase.getstartinggamificationvalues.GetStartingGamificationValuesCommand;
import edu.pjwstk.gamification.usecase.getstartinggamificationvalues.GetStartingGamificationValuesUseCase;
import edu.pjwstk.gamification.usecase.inituserstatistics.InitUserStatisticsCommand;
import edu.pjwstk.gamification.usecase.inituserstatistics.InitUserStatisticsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GamificationApiImpl implements GamificationApi {

    private final InitUserStatisticsUseCase initUserStatisticsUseCase;
    private final GetStartingGamificationValuesUseCase getStartingGamificationValuesUseCase;
    private final GetRequiredExperienceByLevelIdUseCase getRequiredExperienceByLevelIdUseCase;

    @Override
    public void initUserStatisticsFor(UUID userId) {
        initUserStatisticsUseCase.execute(new InitUserStatisticsCommand(userId));
    }

    @Override
    public StartingGamificationValuesDto getStartingGamificationValues() {
        return getStartingGamificationValuesUseCase.execute(new GetStartingGamificationValuesCommand());
    }

    @Override
    public GetRequiredExperienceByLevelIdResult getRequiredExperienceByLevelId(Integer levelId) {
        return getRequiredExperienceByLevelIdUseCase.execute(new GetRequiredExperienceByLevelIdCommand(levelId));
    }
}
