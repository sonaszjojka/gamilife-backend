package pl.gamilife.gamification.application.usecase.processonboardingcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.domain.service.RewardService;

import java.util.List;

@Service
@AllArgsConstructor
@Transactional
public class ProcessOnboardingCompletionUseCaseImpl implements ProcessOnboardingCompletionUseCase {

    private final LevelRepository levelRepository;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessOnboardingCompletionCommand cmd) {
        List<Level> levelAfterTutorial = levelRepository.findLevelAfterTutorial();
        rewardService.rewardUser(
                cmd.userId(),
                levelAfterTutorial.getFirst().getRequiredExperience(),
                0
        );

        return null;
    }
}
