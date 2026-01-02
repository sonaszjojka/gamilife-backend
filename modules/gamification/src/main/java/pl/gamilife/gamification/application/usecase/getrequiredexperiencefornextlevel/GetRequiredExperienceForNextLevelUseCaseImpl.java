package pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.service.LevelService;

import java.util.Optional;

@Service
@AllArgsConstructor
public class GetRequiredExperienceForNextLevelUseCaseImpl implements GetRequiredExperienceForNextLevelUseCase {

    private final LevelService levelService;

    @Override
    public Integer execute(GetRequiredExperienceForNextLevelCommand cmd) {
        Optional<Level> optionalLevel = levelService.getNextLevel(cmd.currentLevel());
        return optionalLevel.map(Level::getRequiredExperience).orElse(null);
    }
}
