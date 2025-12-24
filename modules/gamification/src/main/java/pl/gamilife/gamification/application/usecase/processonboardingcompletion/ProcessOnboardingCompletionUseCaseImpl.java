package pl.gamilife.gamification.application.usecase.processonboardingcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.domain.service.LevelService;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class ProcessOnboardingCompletionUseCaseImpl implements ProcessOnboardingCompletionUseCase {

    private final LevelRepository levelRepository;
    private final LevelService levelService;
    private final UserContext userContext;

    @Override
    public Void execute(ProcessOnboardingCompletionCommand cmd) {
        List<Level> levelAfterTutorial = levelRepository.findLevelAfterTutorial();
        userContext.grantRewardsToUser(
                cmd.userId(),
                levelAfterTutorial.getFirst().getRequiredExperience(),
                0
        );
        levelService.levelUpUser(cmd.userId(), levelAfterTutorial);

        return null;
    }
}
