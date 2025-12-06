package pl.gamilife.gamification.application.usecase.getgamificationvaluesaftertutorial;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.gamification.dto.GamificationValuesDto;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional(readOnly = true)
public class GetGamificationValuesAfterTutorialUseCaseImpl implements GetGamificationValuesAfterTutorialUseCase {

    private LevelRepository levelRepository;

    @Override
    public GamificationValuesDto execute(GetGamificationValuesAfterTutorialCommand cmd) {
        List<Level> level = levelRepository.findLevelAfterTutorial();

        if (level.isEmpty()) {
            throw new IllegalStateException("No starting level found in db");
        }

        return new GamificationValuesDto(
                level.getFirst().getLevel(),
                level.getFirst().getRequiredExperience(),
                0
        );
    }
}
