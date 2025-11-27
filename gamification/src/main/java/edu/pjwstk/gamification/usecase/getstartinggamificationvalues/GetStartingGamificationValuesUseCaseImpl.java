package edu.pjwstk.gamification.usecase.getstartinggamificationvalues;

import edu.pjwstk.api.gamification.dto.StartingGamificationValuesDto;
import edu.pjwstk.gamification.model.Level;
import edu.pjwstk.gamification.repository.LevelRepository;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class GetStartingGamificationValuesUseCaseImpl implements GetStartingGamificationValuesUseCase {

    private final LevelRepository levelRepository;

    @Override
    public StartingGamificationValuesDto executeInternal(GetStartingGamificationValuesCommand cmd) {
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
