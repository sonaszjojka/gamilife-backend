package pl.gamilife.gamification.application.usecase.getstartinggamificationvalues;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;

import java.util.List;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetStartingGamificationValuesUseCaseImpl implements GetStartingGamificationValuesUseCase {

    private final LevelRepository LevelRepository;

    @Override
    public StartingGamificationValuesDto execute(GetStartingGamificationValuesCommand cmd) {
        List<Level> level = LevelRepository.findStartingLevel();

        if (level.isEmpty()) {
            throw new RuntimeException("No starting level found in db");
        }

        return new StartingGamificationValuesDto(
                level.getFirst().getLevel(),
                0,
                0
        );
    }
}
