package pl.gamilife.gamification.application.usecase.getrequiredexperiencefornextlevel;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;

import java.util.Optional;

@Service
@AllArgsConstructor
public class GetRequiredExperienceForNextLevelUseCaseImpl implements GetRequiredExperienceForNextLevelUseCase {

    private final LevelRepository levelRepository;

    @Override
    public Integer execute(GetRequiredExperienceForNextLevelCommand cmd) {
        Optional<Level> optionalLevel = levelRepository.findByLevel(cmd.currentLevel() + 1);
        return optionalLevel.map(Level::getRequiredExperience).orElse(null);
    }
}
