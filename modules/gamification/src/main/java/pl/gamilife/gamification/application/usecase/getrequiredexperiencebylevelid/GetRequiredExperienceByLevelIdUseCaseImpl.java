package pl.gamilife.gamification.application.usecase.getrequiredexperiencebylevelid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.exception.LevelNotFoundException;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;

@Service
@AllArgsConstructor
public class GetRequiredExperienceByLevelIdUseCaseImpl implements GetRequiredExperienceByLevelIdUseCase {

    private final LevelRepository levelRepository;

    @Override
    public Integer execute(GetRequiredExperienceByLevelIdCommand cmd) {
        Level level = levelRepository.findByLevel(cmd.levelId())
                .orElseThrow(() -> new LevelNotFoundException(String.format("Level %s not found", cmd.levelId())));

        return level.getRequiredExperience();
    }
}
