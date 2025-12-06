package edu.pjwstk.gamification.usecase.getrequiredexperiencebylevelid;

import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.gamification.model.Level;
import edu.pjwstk.gamification.repository.LevelRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class GetRequiredExperienceByLevelIdUseCaseImpl implements GetRequiredExperienceByLevelIdUseCase{

    private final LevelRepository levelRepository;

    @Override
    public GetRequiredExperienceByLevelIdResult execute(GetRequiredExperienceByLevelIdCommand cmd) {
        Level level = levelRepository.findByLevel(cmd.levelId());
        return new GetRequiredExperienceByLevelIdResult(level.getRequiredExperience());
    }
}
