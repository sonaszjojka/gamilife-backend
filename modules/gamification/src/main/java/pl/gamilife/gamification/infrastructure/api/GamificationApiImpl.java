package pl.gamilife.gamification.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel.GetRequiredExperienceForNextLevelCommand;
import pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel.GetRequiredExperienceForNextLevelUseCase;

@Service
@AllArgsConstructor
public class GamificationApiImpl implements GamificationApi {

    private final GetRequiredExperienceForNextLevelUseCase getRequiredExperienceForNextLevelUseCase;

    @Override
    public Integer getExperienceRequiredForNextLevel(int level) {
        return getRequiredExperienceForNextLevelUseCase.execute(new GetRequiredExperienceForNextLevelCommand(
                level
        ));
    }

}
