package pl.gamilife.gamification.usecase.getstartinggamificationvalues;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.gamification.model.Level;
import pl.gamilife.gamification.repository.LevelRepository;

import java.util.List;

@Service
@AllArgsConstructor
public class GetStartingGamificationValuesUseCaseImpl implements GetStartingGamificationValuesUseCase {

    private final LevelRepository levelRepository;

    @Override
    public StartingGamificationValuesDto execute(GetStartingGamificationValuesCommand cmd) {
        List<Level> level = levelRepository.findStartingLevel(PageRequest.of(0, 1));

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
